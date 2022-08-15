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

   use type Tensors.Tensor_Shape;

   subtype Vector is Tensor;
   subtype Matrix is Tensor;

   type Update_Phase is (Time_Update, Measurement_Update);

   type Filter_Kind is (Filter_UKF, Filter_CDKF);

   type State_Covariance is private;

   type Weights_Type (N : Positive; Kind : Filter_Kind) is private;

   type Filter (Kind : Filter_Kind; Dimension_X, Dimension_Z : Positive) is tagged private;

   function State (Object : Filter) return Vector
     with Post => State'Result.Shape = (1 => Object.Dimension_X);

   procedure Set_State (Object : in out Filter; State : Vector)
     with Pre => State.Shape = (1 => Object.Dimension_X);

private

   subtype Element_Type is Tensors.Element_Type;

   package Tensor_Holders is new Ada.Containers.Indefinite_Holders (Tensor);

   --  These predicates cannot be applied to the formal types above
   subtype Vector_Holder is Tensor_Holders.Holder;
   subtype Matrix_Holder is Tensor_Holders.Holder;

   type Weights_Type (N : Positive; Kind : Filter_Kind) is record
      Mean           : Vector_Holder;
      Scaling_Factor : Element_Type;
      case Kind is
         when Filter_UKF =>
            Covariance : Vector_Holder;
         when Filter_CDKF =>
            Covariance_1 : Element_Type;
            Covariance_2 : Element_Type;
      end case;
   end record;

   type State_Covariance is record
      State      : Vector_Holder;
      Covariance : Matrix_Holder;
   end record;

   type Filter (Kind : Filter_Kind; Dimension_X, Dimension_Z : Positive) is tagged record
      Process_Noise     : Matrix_Holder;
      Measurement_Noise : Matrix_Holder;

      Weights  : Weights_Type (Dimension_X, Kind);
      Estimate : State_Covariance;
   end record;

   function State (Object : Filter) return Vector is (Object.Estimate.State.Element);

end Orka.Numerics.Kalman;
