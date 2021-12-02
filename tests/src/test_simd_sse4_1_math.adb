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

with AUnit.Assertions;
with AUnit.Test_Caller;

with Orka.SIMD.SSE.Singles;
with Orka.SIMD.SSE4_1.Singles.Math;

package body Test_SIMD_SSE4_1_Math is

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE4_1.Singles.Math;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE4.1 - Math) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Round_Nearest_Integer function", Test_Nearest_Integer'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Floor function", Test_Floor'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Ceil function", Test_Ceil'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Round_Truncate function", Test_Truncate'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Nearest_Integer (Object : in out Test) is
      Elements : constant m128 := (-1.5, 0.6, -0.4, 1.9);
      Expected : constant m128 := (-2.0, 1.0,  0.0, 2.0);

      Result : constant m128 := Round_Nearest_Integer (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Nearest_Integer;

   procedure Test_Floor (Object : in out Test) is
      Elements : constant m128 := (-1.5, 0.6, -0.4, 1.9);
      Expected : constant m128 := (-2.0, 0.0, -1.0, 1.0);

      Result : constant m128 := Floor (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Floor;

   procedure Test_Ceil (Object : in out Test) is
      Elements : constant m128 := (-1.5, 0.2, -0.4, 1.9);
      Expected : constant m128 := (-1.0, 1.0,  0.0, 2.0);

      Result : constant m128 := Ceil (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Ceil;

   procedure Test_Truncate (Object : in out Test) is
      Elements : constant m128 := (-1.5, 0.2, -0.4, 1.9);
      Expected : constant m128 := (-1.0, 0.0,  0.0, 1.0);

      Result : constant m128 := Round_Truncate (Elements);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Truncate;

end Test_SIMD_SSE4_1_Math;
