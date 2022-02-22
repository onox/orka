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

with AUnit.Test_Suites;

with Orka.Transforms.SIMD_Vectors;

generic
   Suite_Name : String;

   with package Vectors is new Orka.Transforms.SIMD_Vectors (<>);
package Generic_Test_Transforms_Vectors is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Generic_Test_Transforms_Vectors;
