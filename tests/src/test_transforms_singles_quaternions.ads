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

package Test_Transforms_Singles_Quaternions is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Multiplication (Object : in out Test);
   procedure Test_Conjugate (Object : in out Test);
   procedure Test_Norm (Object : in out Test);
   procedure Test_Normalize (Object : in out Test);
   procedure Test_Normalized (Object : in out Test);
   procedure Test_Rotate_Axis_Angle (Object : in out Test);
   procedure Test_Rotate_Vectors (Object : in out Test);
   procedure Test_Rotate_At_Origin (Object : in out Test);
   procedure Test_Slerp (Object : in out Test);

end Test_Transforms_Singles_Quaternions;
