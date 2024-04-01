--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Ada.Numerics.Generic_Elementary_Functions;

with Orka.Transforms.Doubles.Matrices;

package body AWT.IMUs is

   use type Orka.Float_64;
   use all type Orka.Index_4D;
   use Kalman.Tensors;

   O_Variance : constant := (1.0e-4) ** 2;
   B_Variance : constant := (8.0e-6) ** 2;
   Z_Variance : constant := (6.0e-3) ** 2;
   P_Scale    : constant := 1.0e-3;

   Unit_Norm_Convergence_Rate : constant := 0.3;
   Acceleration_Threshold     : constant := 0.01;
   Velocity_Threshold         : constant := 0.008;

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Orka.Float_64);

   procedure Update_Bias (Object : in out IMU) is
      Measurement_X, Measurement_Y, Measurement_Z : Kalman.Vector :=
        Empty ([Object.Velocity_Measurements'Length]);
   begin
      for Index in Object.Velocity_Measurements'Range loop
         Measurement_X.Set ([Index], Object.Velocity_Measurements (Index) (X));
         Measurement_Y.Set ([Index], Object.Velocity_Measurements (Index) (Y));
         Measurement_Z.Set ([Index], Object.Velocity_Measurements (Index) (Z));
      end loop;

      declare
         State : Kalman.Vector := Object.Filter.State;
         Bias  : constant Kalman.Vector := To_Tensor
           ([Measurement_X.Mean, Measurement_Y.Mean, Measurement_Z.Mean]);
      begin
         State.Set (Range_Type'(5, 7), Bias);
         Object.Filter.Set_State (State);
      end;
   end Update_Bias;

   function Get_New_Orientation
     (Orientation : Kalman.Vector;
      Velocity    : Kalman.Vector;
      DT          : Orka.Float_64) return Kalman.Vector
   is
      W : constant Kalman.Vector := Velocity * DT;

      --  [1] "Sigma-Point Kalman Filters for Probabilistic Inference in
      --      Dynamic State-Space Models", van der Merwe R.,
      --      Oregon Health & Science University, 2004

      --  Similar to equation 5.28 from [1], except the 3x3 skew-symmetric
      --  matrix has been moved from the lower right to the upper left
      --  because the scalar component of Quaternion is at the 4th position
      Omega : constant Kalman.Matrix :=
        To_Tensor ([0.0, -W (3),  W (2), -W (1),
                  W (3),    0.0, -W (1), -W (2),
                 -W (2),  W (1),    0.0, -W (3),
                  W (1),  W (2),  W (3),   0.0], Shape => [4, 4]);

      Y : constant Orka.Float_64 := 1.0 - Orka.Float_64'(Orientation * Orientation);
      J : constant Orka.Float_64 := Unit_Norm_Convergence_Rate / DT;
      --  J is convergence speed of numerical error with J * DT < 1.0 [1]

      --  Equation 5.30 from [1]
      S : constant Orka.Float_64 := 0.5 * W.Norm;

      --  Equation 5.31 from [1]
      Delta_Orientation : constant Kalman.Vector :=
        Identity (4) * (EF.Cos (S) + J * DT * Y) - 0.5 * Omega * (EF.Sin (S) / S);
   begin
      return Delta_Orientation * Orientation;
   end Get_New_Orientation;

   procedure Integrate
     (Object       : in out IMU;
      Velocity     : Vectors.Direction;
      Acceleration : Vectors.Vector4;
      DT           : Duration;
      State        : out Estimated_State;
      Calibrated   : out Boolean)
   is
      Measured_Velocity : constant Kalman.Vector :=
        To_Tensor ([Velocity (X), Velocity (Y), Velocity (Z)]);

      Measured_Acceleration : constant Kalman.Vector :=
        To_Tensor ([Acceleration (X), Acceleration (Y), Acceleration (Z)]);

      No_Acceleration : constant Boolean :=
        abs (1.0 - Measured_Acceleration.Norm) < Acceleration_Threshold;

      function F (Point : Kalman.Vector; DT : Orka.Float_64) return Kalman.Vector is
         Old_Orientation : constant Kalman.Vector := Point (Range_Type'(1, 4));
         Bias            : constant Kalman.Vector := Point (Range_Type'(5, 7));
      begin
         return Get_New_Orientation (Old_Orientation, Measured_Velocity - Bias, DT) & Bias;
      end F;

      function H (Point : Kalman.Vector) return Kalman.Vector is
         Orientation : constant Quaternions.Quaternion :=
           [Point (1), Point (2), Point (3), Point (4)];

         use Orka.Transforms.Doubles.Matrices;

         Gravity_NED  : constant Vectors.Vector4 := [0.0, -1.0, 0.0, 0.0];
         Gravity_Body : constant Vectors.Vector4 :=
           R (Quaternion => Vectors.Vector4 (Quaternions.Normalize (Orientation))) * Gravity_NED;

         Predicted_Acceleration : constant Kalman.Vector :=
           To_Tensor ([Gravity_Body (X), Gravity_Body (Y), Gravity_Body (Z)]);
      begin
         --  Returning the measurement fools the filter into thinking the predicted
         --  state (prior) is already equal to the measurement, thus no information
         --  from the measurement will be added to the prior when computing the posterior.
         --  It is an alternative for having an infinite variance in the measurement noise matrix.
         return (if No_Acceleration then Predicted_Acceleration else Measured_Acceleration);
      end H;
   begin
      if Object.Velocity_Index < Object.Velocity_Measurements'Last then
         if No_Acceleration and then Measured_Velocity.Norm <= Velocity_Threshold then
            Object.Velocity_Index := Object.Velocity_Index + 1;
            Object.Velocity_Measurements (Object.Velocity_Index) := Velocity;
         end if;
         Calibrated := False;
      else
         Object.Update_Bias;
         Object.Velocity_Index := 0;
         Calibrated := True;
      end if;

      CDKF.Predict_Update (Object.Filter, F'Access, H'Access, DT, Measured_Acceleration);

      declare
         S           : constant Kalman.Vector          := Object.Filter.State;
         Orientation : constant Quaternions.Quaternion := [S (1), S (2), S (3), S (4)];
         Bias        : constant Vectors.Direction      := [S (5), S (6), S (7), 0.0];

         use type Vectors.Direction;
      begin
         State.Orientation      := Quaternions.Normalize (Orientation);
         State.Angular_Velocity := Velocity - Bias;
      end;
   end Integrate;

   procedure Initialize
     (Object      : in out IMU;
      Orientation : Quaternions.Quaternion := Quaternions.Identity) is
   begin
      Object.Filter := CDKF.Create_Filter
        (X       => To_Tensor
           ([Orientation (X), Orientation (Y), Orientation (Z), Orientation (W), 0.0, 0.0, 0.0]),
         P       => P_Scale * Identity (7),
         Q       => Diagonal ([1 .. 4 => O_Variance,
                               5 .. 7 => B_Variance]),
         R       => Diagonal ([1 .. 3 => Z_Variance]),
         Weights => CDKF.Weights (N => 7, H2 => 3.0));
      Object.Velocity_Index := 0;
   end Initialize;

   procedure Reset
     (Object      : in out IMU;
      Orientation : Quaternions.Quaternion := Quaternions.Identity)
   is
      State : Kalman.Vector := Object.Filter.State;

      Tensor_Orientation : constant Kalman.Vector :=
        To_Tensor ([Orientation (X), Orientation (Y), Orientation (Z), Orientation (W)]);
   begin
      State.Set (Range_Type'(1, 4), Tensor_Orientation);
      Object.Filter.Set_State (State);
   end Reset;

end AWT.IMUs;
