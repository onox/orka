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

package body Orka.Numerics.Kalman.SPKF is

   function Points
     (Estimate : State_Covariance;
      Weights  : Weights_Type) return Matrix
   is
      X : Vector renames Estimate.State.Constant_Reference;
      P : Matrix renames Estimate.Covariance.Constant_Reference;

      N : constant Natural := X.Elements;

      Sigmas : Matrix := Empty ((2 * N + 1, N));

      LT : constant Matrix := Points_Covariance (Weights.Scaling_Factor * P).Transpose;
      --  Take transpose of lower triangular Cholesky matrix so that the
      --  rows instead of columns can be used below
   begin
      Sigmas.Set (1, X);

      for I in 1 .. N loop
         declare
            V : constant Vector := LT.Get (I);
         begin
            Sigmas.Set (I + 1,     X + V);
            Sigmas.Set (I + 1 + N, X - V);
         end;
      end loop;

      return Sigmas;
   end Points;

   function Apply_F
     (N      : Positive;
      F      : not null access function (Point : Vector; DT : Orka.Float_64) return Vector;
      Points : Kalman.Matrix;
      DT     : Orka.Float_64) return Kalman.Matrix is
   begin
      return Result : Matrix := Empty ((Points.Rows, N)) do
         for Index in 1 .. Points.Rows loop
            Result.Set (Index, F (Points.Get (Index), DT));
         end loop;
      end return;
   end Apply_F;

   function Apply_H
     (N      : Positive;
      H      : not null access function (Point : Vector) return Vector;
      Points : Kalman.Matrix) return Kalman.Matrix is
   begin
      return Result : Kalman.Matrix := Empty ((Points.Rows, N)) do
         for Index in 1 .. Points.Rows loop
            Result.Set (Index, H (Points.Get (Index)));
         end loop;
      end return;
   end Apply_H;

   procedure Predict_Update
     (Object      : in out Filter;
      F           : not null access function (Point : Vector; DT : Orka.Float_64) return Vector;
      H           : not null access function (Point : Vector) return Vector;
      DT          : Duration;
      Measurement : Vector)
   is
      Q : Matrix renames Object.Process_Noise.Constant_Reference;
      R : Matrix renames Object.Measurement_Noise.Constant_Reference;

      --  Compute transformed sigma points (Y)
      Y : constant Matrix :=
        Apply_F (Object.Dimension_X, F, Points (Object.Estimate, Object.Weights), Float_64 (DT));
      pragma Assert (Y.Shape = (2 * Object.Weights.N + 1, Object.Dimension_X));

      --  Compute predicted x and P (prior) of transformed sigma points Y
      Prediction_State : constant State_Covariance :=
        Transform (Time_Update, Y, Q, Object.Weights);

      -------------------------------------------------------------------------

      --  Compute new sigma points and convert them to measurement space (Z)
      Z : constant Matrix :=
        Apply_H (Object.Dimension_Z, H, Points (Prediction_State, Object.Weights));
      pragma Assert (Z.Shape = (2 * Object.Weights.N + 1, Object.Dimension_Z));

      --  Compute x and P of Z
      Measurement_State : constant State_Covariance :=
        Transform (Measurement_Update, Z, R, Object.Weights);

      --  Compute cross covariance
      Pxz : constant Matrix :=
        Cross_Covariance (Y, Z, Prediction_State, Measurement_State, Object.Weights);
      pragma Assert (Pxz.Shape = (Object.Dimension_X, Object.Dimension_Z));

      --  Compute Kalman gain
      K : constant Matrix := Kalman_Gain (Measurement_State, Pxz);

      --  Mean of prediction (prior) and measurement
      Mean_X : Vector renames Prediction_State.State.Constant_Reference;
      Mean_Y : Vector renames Measurement_State.State.Constant_Reference;
   begin
      --  Compute state x and P (posterior/estimate)
      Object.Estimate.State.Replace_Element (Mean_X + Vector'(K * (Measurement - Mean_Y)));
      Object.Estimate.Covariance.Replace_Element
        (Posterior_Covariance (Prediction_State, Measurement_State, K));
   end Predict_Update;

end Orka.Numerics.Kalman.SPKF;
