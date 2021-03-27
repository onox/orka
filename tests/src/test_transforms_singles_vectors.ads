--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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
with AUnit.Test_Fixtures;

package Test_Transforms_Singles_Vectors is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Add (Object : in out Test);
   procedure Test_Subtract (Object : in out Test);
   procedure Test_Scale (Object : in out Test);
   procedure Test_Absolute (Object : in out Test);
   procedure Test_Magnitude (Object : in out Test);
   procedure Test_Normalize (Object : in out Test);
   procedure Test_Distance (Object : in out Test);
   procedure Test_Projection (Object : in out Test);
   procedure Test_Perpendicular (Object : in out Test);
   procedure Test_Angle (Object : in out Test);
   procedure Test_Dot_Product (Object : in out Test);
   procedure Test_Cross_Product (Object : in out Test);

end Test_Transforms_Singles_Vectors;
