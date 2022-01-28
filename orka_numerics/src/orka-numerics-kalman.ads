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

private with Ada.Containers.Indefinite_Holders;

with Orka.Numerics.Tensors;

generic
   with package Tensors is new Orka.Numerics.Tensors (<>);
   type Tensor (<>) is new Tensors.Tensor with private;
package Orka.Numerics.Kalman is
   pragma Preelaborate;

   use type Tensors.Tensor_Dimension;
   use type Tensors.Tensor_Shape;
   use type Tensors.Element_Type;

   subtype Vector is Tensor;
   subtype Matrix is Tensor;

   type Weights_Type (N : Positive) is private;

   function Weights (N : Positive; A, B, K : Tensors.Element_Type) return Weights_Type
     with Pre => A in 0.0 .. 1.0 and B >= 0.0 and K >= 0.0;
   --  Return a set of weights for the sigma points for the UKF
   --
   --  A should be small to avoid non-local effects. B is used to reduce
   --  higher-order errors. For a Gaussian, use B = 2 and K = 3 - N.

   type Filter (Dimension_X, Dimension_Z : Positive) is tagged private;

   function Create_Filter
     (X       : Vector;
      P, Q, R : Matrix;
      Weights : Weights_Type) return Filter
   with Pre => X.Dimensions = 1
                 and X.Rows = P.Rows
                 and X.Rows = Weights.N
                 and Q.Is_Square and P.Shape = Q.Shape
                 and R.Is_Square,
        Post => Create_Filter'Result.Dimension_X = Q.Rows and
                Create_Filter'Result.Dimension_Z = R.Rows and
                Create_Filter'Result.State = X;

   function Create_Filter
     (Q, R    : Matrix;
      Weights : Weights_Type) return Filter
   is (Create_Filter
         (X => Zeros (Weights.N),
          P => Identity (Weights.N),
          Q => Q,
          R => R,
          Weights => Weights));

   procedure Predict_Update
     (Object      : in out Filter;
      F           : not null access function (Points : Matrix; DT : Duration) return Matrix;
      H           : not null access function (Points : Matrix) return Matrix;
      DT          : Duration;
      Measurement : Vector)
   with Pre => Measurement.Shape = (1 => Object.Dimension_Z);

   function State (Object : Filter) return Vector
     with Post => State'Result.Shape = (1 => Object.Dimension_X);

private

   subtype Element_Type is Tensors.Element_Type;

   package Tensor_Holders is new Ada.Containers.Indefinite_Holders (Tensor);

   --  These predicates cannot be applied to the formal types above
   subtype Vector_Holder is Tensor_Holders.Holder
     with Dynamic_Predicate => Tensor_Holders.Constant_Reference (Vector_Holder).Dimensions = 1;
   subtype Matrix_Holder is Tensor_Holders.Holder
     with Dynamic_Predicate => Tensor_Holders.Constant_Reference (Matrix_Holder).Dimensions = 2;

   type Weights_Type (N : Positive) is record
      Mean, Covariance : Vector_Holder;
      Scaling_Squared  : Element_Type;
   end record;

   type State_Covariance is record
      State      : Vector_Holder;
      Covariance : Matrix_Holder;
   end record;

   type Filter (Dimension_X, Dimension_Z : Positive) is tagged record
      Process_Noise     : Matrix_Holder;
      Measurement_Noise : Matrix_Holder;

      Weights  : Weights_Type (Dimension_X);
      Estimate : State_Covariance;
   end record;

end Orka.Numerics.Kalman;
