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

generic
package Orka.Numerics.Kalman.CDKF is
   pragma Preelaborate;

   use type Tensors.Tensor_Axis;
   use type Tensors.Element_Type;

   --  This package implements the state estimation form with additive noise
   --  of the square-root Central Difference Kalman Filter, Algorithm 13 from [1]
   --
   --  (See package Orka.Numerics.Kalman.SPKF for reference [1])

   function Weights (N : Positive; H2 : Tensors.Element_Type) return Weights_Type
     with Pre => H2 >= 1.0;
   --  Return a set of weights for the sigma points for the CDKF
   --
   --  H2 is the square of the interval size and should be chosen such
   --  that it is equal to the kurtosis of the state's distribution. The
   --  kurtosis is equal to the excess kurtosis + 3. For a Gaussian (normal
   --  distribution), the excess kurtosis is 0, so H2 = 3.

   function Create_Filter
     (X       : Vector;
      P, Q, R : Matrix;
      Weights : Weights_Type) return Filter
   with Pre => X.Axes = 1
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
      F           : not null access function (Point : Vector; DT : Orka.Float_64) return Vector;
      H           : not null access function (Point : Vector) return Vector;
      DT          : Duration;
      Measurement : Vector);

end Orka.Numerics.Kalman.CDKF;
