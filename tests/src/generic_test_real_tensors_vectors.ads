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

with AUnit.Test_Cases;
with AUnit.Test_Suites;

with Orka.Numerics.Tensors;

generic
   Suite_Name : String;
   Large_Data : Boolean;

   type Abstract_Test_Case is abstract new AUnit.Test_Cases.Test_Case with private;

   type Element_Type is digits <>;
   with package Tensors is new Orka.Numerics.Tensors (Element_Type, Element_Type, Float_Type, <>, <>, <>, <>, <>);
--   with package Tensors is new Orka.Numerics.Tensors (<>);

   type Tensor_Type (<>) is new Tensors.Real_Tensor with private;

   with procedure Reset_Random (Seed : Duration);
package Generic_Test_Real_Tensors_Vectors is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test_Case is new Abstract_Test_Case with null record;

   overriding
   procedure Register_Tests (Object : in out Test_Case);

   overriding
   function Name (Object : Test_Case) return AUnit.Test_String;

end Generic_Test_Real_Tensors_Vectors;
