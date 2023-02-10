--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

with Orka.Numerics.Tensors;
with Orka.Resources.Locations;

generic
   Suite_Name : String;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with private;

   with package Tensors is new Orka.Numerics.Tensors (<>);

   type Tensor_Type (<>) is new Tensors.Tensor with private;

   type QR_Factorization_Type (<>) is new Tensors.QR_Factorization with private;

   with procedure Initialize_Shaders
     (Prefix_Sum, Tensors_GPU : Orka.Resources.Locations.Location_Ptr) is null;

   with function Q (Object : QR_Factorization_Type'Class) return Tensor_Type is <>;
   with function R (Object : QR_Factorization_Type'Class) return Tensor_Type is <>;

package Generic_Test_Tensors_Matrices is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Generic_Test_Tensors_Matrices;
