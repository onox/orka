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

with Orka.Numerics.Kalman.SPKF;

package body Orka.Numerics.Kalman.CDKF is

   function Weights (N : Positive; H2 : Tensors.Element_Type) return Weights_Type is
      L : constant Element_Type := Element_Type (N);

      --  See equation 3.85 in Section 3.4.1 of [1]

      Count : constant Positive := 2 * N + 1;

      Weights_Mean : Vector := Fill ((1 => Count), 1.0 / (2.0 * H2));

      Weights_Cov_1 : constant Element_Type := 1.0 / (4.0 * H2);
      Weights_Cov_2 : constant Element_Type := (H2 - 1.0) / (4.0 * H2**2);
   begin
      Weights_Mean.Set ((1 => 1), (H2 - L) / H2);

      return
        (N              => N,
         Kind           => Filter_CDKF,
         Mean           => Tensor_Holders.To_Holder (Weights_Mean),
         Covariance_1   => Weights_Cov_1,
         Covariance_2   => Weights_Cov_2,
         Scaling_Factor => Tensors.Square_Root (H2));
   end Weights;

   function Create_Filter
     (X       : Vector;
      P, Q, R : Matrix;
      Weights : Weights_Type) return Filter is
   begin
      return Result : Filter (Weights.Kind, Dimension_X => Q.Rows, Dimension_Z => R.Rows) do
         Result.Process_Noise     := Tensor_Holders.To_Holder (Cholesky (Q));
         Result.Measurement_Noise := Tensor_Holders.To_Holder (Cholesky (R));

         Result.Weights  := Weights;
         Result.Estimate :=
           (State      => Tensor_Holders.To_Holder (X),
            Covariance => Tensor_Holders.To_Holder (Cholesky (P, Tensors.Upper)));
      end return;
   end Create_Filter;

   ----------------------------------------------------------------------------

   function Points_Covariance (Covariance : Matrix) return Matrix is (Covariance);

   function Duplicate (N : Positive; Row : Vector) return Matrix is
   begin
      return Result : Matrix := Empty ((N, Row.Elements)) do
         for I in 1 .. N loop
            Result.Set (I, Row);
         end loop;
      end return;
   end Duplicate;

   function CDKF_Covariance
     (N                    : Positive;
      Phase                : Update_Phase;
      Points, S            : Matrix;
      Weights_1, Weights_2 : Element_Type) return Matrix
   is
      Y_1L  : constant Matrix := Points.Get (Tensors.Range_Type'(2, N + 1));
      Y_L2L : constant Matrix := Points.Get (Tensors.Range_Type'(N + 2, 2 * N + 1));
      Y_1   : constant Matrix := Duplicate (N, Points.Get (1));

      V1 : constant Vector := Y_1L - Y_L2L;
      V2 : constant Vector :=
         (case Phase is
            when Time_Update        => Y_1L + Y_L2L - 2.0 * Y_1,
            when Measurement_Update => Y_1L - Y_L2L - 2.0 * Y_1);

      M1 : constant Matrix := Tensors.Square_Root (Weights_1) * V1;
      M2 : constant Matrix := Tensors.Square_Root (Weights_2) * V2;
   begin
      return Matrix'(M1 & M2 & S).QR;
   end CDKF_Covariance;

   function Transform
     (Phase         : Update_Phase;
      Points, Noise : Matrix;
      Weights       : Weights_Type) return State_Covariance
   is
      U : constant Vector := Matrix'(Weights.Mean.Constant_Reference * Points).Flatten;
   begin
      return
        (State      => Tensor_Holders.To_Holder (U),
         Covariance => Tensor_Holders.To_Holder
           (CDKF_Covariance
              (Weights.N, Phase, Points, Noise,
               Weights.Covariance_1,
               Weights.Covariance_2)));
   end Transform;

   function CDKF_Cross_Covariance
     (N         : Positive;
      P, Y      : Matrix;
      Weights_1 : Element_Type) return Matrix
   is
      Y_1L  : Matrix renames Y.Get (Tensors.Range_Type'(2, N + 1));
      Y_L2L : Matrix renames Y.Get (Tensors.Range_Type'(N + 2, 2 * N + 1));

      Root_Weight_Times_P : constant Matrix := Tensors.Square_Root (Weights_1) * P;
   begin
      return Root_Weight_Times_P * (Y_1L - Y_L2L);
   end CDKF_Cross_Covariance;

   function Cross_Covariance
     (X, Y             : Matrix;
      State_X, State_Y : State_Covariance;
      Weights          : Weights_Type) return Matrix
   is
      Pp : Matrix renames State_X.Covariance.Constant_Reference;
   begin
      return CDKF_Cross_Covariance (Weights.N, Pp, Y, Weights.Covariance_1);
   end Cross_Covariance;

   function Kalman_Gain (State_Y : State_Covariance; Cross_Covariance : Matrix) return Matrix is
      Pz : Matrix renames State_Y.Covariance.Constant_Reference;
   begin
      return Divide_By
        (Divide_By (Cross_Covariance, Pz.Transpose, Tensors.Lower),
         Pz, Tensors.Upper);
   end Kalman_Gain;

   function Posterior_Covariance
     (State_X, State_Y : State_Covariance;
      Kalman_Gain      : Matrix) return Matrix
   is
      Pp : Matrix renames State_X.Covariance.Constant_Reference;
      Pz : Matrix renames State_Y.Covariance.Constant_Reference;

      U : constant Matrix := Transpose (Kalman_Gain * Pz);  --  Transpose to quickly fetch rows
   begin
      return Result : Matrix := Pp do
         for I in 1 .. U.Rows loop
            begin
               Result := Cholesky_Update (Result, U.Get (I), Tensors.Downdate);
            exception
               when Not_Positive_Definite_Matrix =>
                  null;
            end;
         end loop;
      end return;
   end Posterior_Covariance;

   ----------------------------------------------------------------------------

   package SPKF_Filter is new SPKF
     (Points_Covariance, Transform, Cross_Covariance, Kalman_Gain, Posterior_Covariance);

   procedure Predict_Update
     (Object      : in out Filter;
      F           : not null access function (Point : Vector; DT : Orka.Float_64) return Vector;
      H           : not null access function (Point : Vector) return Vector;
      DT          : Duration;
      Measurement : Vector) renames SPKF_Filter.Predict_Update;

end Orka.Numerics.Kalman.CDKF;
