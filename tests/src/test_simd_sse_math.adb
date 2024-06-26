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

with Orka.SIMD.SSE.Singles.Math;

package body Test_SIMD_SSE_Math is

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Math;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE - Math) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Max function", Test_Max'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Min function", Test_Min'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Max (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, -2.0, 1.0];
      Right : constant m128 := [0.0, 0.0, 1.0, 2.0];

      Expected : constant m128 := [0.0, 1.0, 1.0, 2.0];
      Result   : constant m128 := Max (Left, Right);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Max;

   procedure Test_Min (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, -2.0, 1.0];
      Right : constant m128 := [0.0, 0.0, 1.0, 2.0];

      Expected : constant m128 := [0.0, 0.0, -2.0, 1.0];
      Result   : constant m128 := Min (Left, Right);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_4D'Image (I));
      end loop;
   end Test_Min;

end Test_SIMD_SSE_Math;
