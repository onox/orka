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

package body Orka.Numerics.Kalman.UKF is

   function Weights (N : Positive; A, B, K : Tensors.Element_Type) return Weights_Type is
      L : constant Element_Type := Element_Type (N);

      --  See equation 3.11 in Section 3.2.2 of [1]
      Lambda     : constant Element_Type := A**2 * (L + K) - L;
      Mean_First : constant Element_Type := Lambda / (L + Lambda);

      Count : constant Positive := 2 * N + 1;

      Weights_Mean, Weights_Cov : Vector := Fill ((1 => Count), 1.0 / (2.0 * (L + Lambda)));
   begin
      Weights_Mean.Set ((1 => 1), Mean_First);
      Weights_Cov.Set  ((1 => 1), Mean_First + 1.0 - A**2 + B);

      return
        (N              => N,
         Kind           => Filter_UKF,
         Mean           => Tensor_Holders.To_Holder (Weights_Mean),
         Covariance     => Tensor_Holders.To_Holder (Weights_Cov),
         Scaling_Factor => L + Lambda);
   end Weights;

   function Create_Filter
     (X       : Vector;
      P, Q, R : Matrix;
      Weights : Weights_Type) return Filter is
   begin
      return Result : Filter (Weights.Kind, Dimension_X => Q.Rows, Dimension_Z => R.Rows) do
         Result.Process_Noise     := Tensor_Holders.To_Holder (Q);
         Result.Measurement_Noise := Tensor_Holders.To_Holder (R);

         Result.Weights  := Weights;
         Result.Estimate :=
           (State      => Tensor_Holders.To_Holder (X),
            Covariance => Tensor_Holders.To_Holder (P));
      end return;
   end Create_Filter;

   ----------------------------------------------------------------------------

   function Points_Covariance (Covariance : Matrix) return Matrix is (Cholesky (Covariance));

   function UKF_Covariance
     (Points_Left, Points_Right : Matrix;
      Mean_Left, Mean_Right     : Vector;
      Weights                   : Vector) return Matrix
   is
      P : Matrix := Zeros ((Points_Left.Columns, Points_Right.Columns));
   begin
      for I in 1 .. Points_Left.Rows loop
         declare
            Left  : Vector renames Points_Left.Get (I);
            Right : Vector renames Points_Right.Get (I);

            Weight : Element_Type renames Weights.Get (I);
         begin
            P := P + Weight * Outer (Left - Mean_Left, Right - Mean_Right);
         end;
      end loop;

      return P;
   end UKF_Covariance;

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
           (UKF_Covariance (Points, Points, U, U, Weights.Covariance.Constant_Reference) + Noise));
   end Transform;

   function Cross_Covariance
     (X, Y             : Matrix;
      State_X, State_Y : State_Covariance;
      Weights          : Weights_Type) return Matrix
   is
      Mean_X : Vector renames State_X.State.Constant_Reference;
      Mean_Y : Vector renames State_Y.State.Constant_Reference;
   begin
      return UKF_Covariance (X, Y, Mean_X, Mean_Y, Weights.Covariance.Constant_Reference);
   end Cross_Covariance;

   function Kalman_Gain (State_Y : State_Covariance; Cross_Covariance : Matrix) return Matrix is
     (Cross_Covariance * State_Y.Covariance.Constant_Reference.Inverse);

   function Posterior_Covariance
     (State_X, State_Y : State_Covariance;
      Kalman_Gain      : Matrix) return Matrix
   is
      Pp : Matrix renames State_X.Covariance.Constant_Reference;
      Pz : Matrix renames State_Y.Covariance.Constant_Reference;
   begin
      return Pp - Matrix'(Kalman_Gain * Matrix'(Pz * Kalman_Gain.Transpose));
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

end Orka.Numerics.Kalman.UKF;
