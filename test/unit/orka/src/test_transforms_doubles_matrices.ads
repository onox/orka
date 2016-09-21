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

with Ahven.Framework;

package Test_Transforms_Doubles_Matrices is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   procedure Test_T;
   procedure Test_Rx;
   procedure Test_Ry;
   procedure Test_Rz;
   procedure Test_R;
   procedure Test_S;
   procedure Test_Add_Offset;
   procedure Test_Multiply_Factor;
   procedure Test_Rotate_At_Origin;
   procedure Test_Rotate;
   procedure Test_Rotate_X_At_Origin;
   procedure Test_Rotate_Y_At_Origin;
   procedure Test_Rotate_Z_At_Origin;
   procedure Test_Rotate_X;
   procedure Test_Rotate_Y;
   procedure Test_Rotate_Z;
   procedure Test_Translate;
   procedure Test_Scale_Factors;
   procedure Test_Scale_Factor;
   procedure Test_Transpose;

end Test_Transforms_Doubles_Matrices;
