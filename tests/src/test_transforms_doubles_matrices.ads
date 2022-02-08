--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package Test_Transforms_Doubles_Matrices is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_T (Object : in out Test);
   procedure Test_Rx (Object : in out Test);
   procedure Test_Ry (Object : in out Test);
   procedure Test_Rz (Object : in out Test);
   procedure Test_R (Object : in out Test);
   procedure Test_S (Object : in out Test);
   procedure Test_Add_Offset (Object : in out Test);
   procedure Test_Multiply_Factor (Object : in out Test);
   procedure Test_Rotate_At_Origin (Object : in out Test);
   procedure Test_Rotate (Object : in out Test);
   procedure Test_Rotate_X_At_Origin (Object : in out Test);
   procedure Test_Rotate_Y_At_Origin (Object : in out Test);
   procedure Test_Rotate_Z_At_Origin (Object : in out Test);
   procedure Test_Rotate_X (Object : in out Test);
   procedure Test_Rotate_Y (Object : in out Test);
   procedure Test_Rotate_Z (Object : in out Test);
   procedure Test_Translate (Object : in out Test);
   procedure Test_Scale_Factors (Object : in out Test);
   procedure Test_Scale_Factor (Object : in out Test);
   procedure Test_Transpose (Object : in out Test);
   procedure Test_Main_Diagonal (Object : in out Test);
   procedure Test_Trace (Object : in out Test);

end Test_Transforms_Doubles_Matrices;
