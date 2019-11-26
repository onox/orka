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

with Ahven; use Ahven;

with GL.Types;

with Orka.SIMD.SSE.Singles.Logical;

package body Test_SIMD_SSE_Logical is

   use GL.Types;

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Logical;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Logical");

      T.Add_Test_Routine (Test_And'Access, "Test 'and' operator");
      T.Add_Test_Routine (Test_Or'Access, "Test 'or' operator");
   end Initialize;

   procedure Test_And is
      A : constant Single := 2#0.1000_0100_0010_0001_1000_0100_0010_0001#;
      B : constant Single := 2#0.1000_1001_1001_1001_1001_1001_1001_1001#;

      C : constant Single := 2#0.1000_0000_0000_0001_1000_0000_0000_0001#;

      Left  : constant m128 := (A, 0.0, 1.0, 2.0);
      Right : constant m128 := (B, 1.0, 0.0, 2.0);
 
      Expected : constant m128 := (C, 0.0, 0.0, 2.0);
      Result   : constant m128 := Left and Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_And;

   procedure Test_Or is
      A : constant Single := 2#0.1000_0100_0010_0001_1000_0100_0010_0001#;
      B : constant Single := 2#0.1000_1001_1001_1001_1001_1001_1001_1001#;

      C : constant Single := 2#0.1000_1101_1011_1001_1001_1101_1011_1001#;

      Left  : constant m128 := (A, 0.0, 1.0, 2.0);
      Right : constant m128 := (B, 1.0, 0.0, 2.0);
 
      Expected : constant m128 := (C, 1.0, 1.0, 2.0);
      Result   : constant m128 := Left or Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Or;
 
end Test_SIMD_SSE_Logical;
