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

package body Orka.Numerics.Kalman is

   --  [1] "Sigma-Point Kalman Filters for Probabilistic Inference in
   --      Dynamic State-Space Models", van der Merwe R.,
   --      Oregon Health & Science University, 2004

   function Weights (N : Positive; A, B, K : Tensors.Element_Type) return Weights_Type is
      L : constant Element_Type := Element_Type (N);

      --  See equation 3.11 in Section 3.2.2 of [1]
      Lambda     : constant Element_Type := A**2 * (L + K) - L;
      Mean_First : constant Element_Type := Lambda / (L + Lambda);

      Count : constant Natural := 2 * N + 1;

      Weights_Mean, Weights_Cov : Vector := Fill ((1 => Count), 1.0 / (2.0 * (L + Lambda)));
   begin
      Weights_Mean.Set ((1 => 1), Mean_First);
      Weights_Cov.Set  ((1 => 1), Mean_First + 1.0 - A**2 + B);

      return
        (N               => N,
         Mean            => Tensor_Holders.To_Holder (Weights_Mean),
         Covariance      => Tensor_Holders.To_Holder (Weights_Cov),
         Scaling_Squared => L + Lambda);
   end Weights;

   function Points
     (Estimate : State_Covariance;
      Weights  : Weights_Type) return Matrix
   is
      X : Vector renames Estimate.State.Constant_Reference;
      P : Matrix renames Estimate.Covariance.Constant_Reference;

      N : constant Natural := X.Elements;

      Sigmas : Matrix := Empty ((2 * N + 1, N));

      LT : constant Matrix := Cholesky (Weights.Scaling_Squared * P).Transpose;
      --  Take transpose of lower triangular Cholesky matrix so that
      --  the rows instead of column can be used below
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

   function UT_Covariance
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
   end UT_Covariance;

   function Unscented_Transform
     (Y, Q    : Matrix;
      Weights : Weights_Type) return State_Covariance
   is
      U : constant Vector := Matrix'(Weights.Mean.Constant_Reference * Y).Flatten;
   begin
      return
        (State      => Tensor_Holders.To_Holder (U),
         Covariance => Tensor_Holders.To_Holder
           (UT_Covariance (Y, Y, U, U, Weights.Covariance.Constant_Reference) + Q));
   end Unscented_Transform;

   function Create_Filter
     (X       : Vector;
      P, Q, R : Matrix;
      Weights : Weights_Type) return Filter is
   begin
      return Result : Filter (Dimension_X => Q.Rows, Dimension_Z => R.Rows) do
         Result.Process_Noise     := Tensor_Holders.To_Holder (Q);
         Result.Measurement_Noise := Tensor_Holders.To_Holder (R);
         Result.Weights := Weights;
         Result.Estimate :=
           (State      => Tensor_Holders.To_Holder (X),
            Covariance => Tensor_Holders.To_Holder (P));
      end return;
   end Create_Filter;

   procedure Predict_Update
     (Object      : in out Filter;
      F           : not null access function (Points : Matrix; DT : Duration) return Matrix;
      H           : not null access function (Points : Matrix) return Matrix;
      DT          : Duration;
      Measurement : Vector)
   is
      Q : Matrix renames Object.Process_Noise.Constant_Reference;
      R : Matrix renames Object.Measurement_Noise.Constant_Reference;

      --  Compute transformed sigma points (Y)
      Y : constant Matrix := F (Points (Object.Estimate, Object.Weights), DT);
      pragma Assert (Y.Shape = (2 * Object.Weights.N + 1, Object.Dimension_X));

      --  Compute predicted x and P (prior) of transformed sigma points Y
      Prediction_State : constant State_Covariance := Unscented_Transform (Y, Q, Object.Weights);

      --  Compute new sigma points and convert them to measurement space (Z)
      Z : constant Matrix := H (Points (Prediction_State, Object.Weights));
      pragma Assert (Z.Shape = (2 * Object.Weights.N + 1, Object.Dimension_Z));

      --  Compute x and P of Z
      Measurement_State : constant State_Covariance := Unscented_Transform (Z, R, Object.Weights);

      --  Prediction
      xp : Vector renames Prediction_State.State.Constant_Reference;
      Pp : Matrix renames Prediction_State.Covariance.Constant_Reference;

      --  Measurement
      uz : Vector renames Measurement_State.State.Constant_Reference;
      Pz : Matrix renames Measurement_State.Covariance.Constant_Reference;

      --  Compute cross covariance
      Pxz : constant Matrix :=
        UT_Covariance (Y, Z, xp, uz, Object.Weights.Covariance.Constant_Reference);
      pragma Assert (Pxz.Shape = (Object.Dimension_X, Object.Dimension_Z));

      --  Compute Kalman gain
      K : constant Matrix := Pxz * Pz.Inverse;
   begin
      --  Compute state x and P (posterior/estimate)
      Object.Estimate.State.Replace_Element      (xp + Vector'(K * (Measurement - uz)));
      Object.Estimate.Covariance.Replace_Element (Pp - Matrix'(K * Matrix'(Pz * K.Transpose)));
   end Predict_Update;

   function State (Object : Filter) return Vector is (Object.Estimate.State.Element);

end Orka.Numerics.Kalman;
