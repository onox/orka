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

with Orka.SIMD.SSE.Singles.Math;

package body Test_SIMD_SSE_Math is

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Math;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Math");

      T.Add_Test_Routine (Test_Max'Access, "Test Max function");
      T.Add_Test_Routine (Test_Min'Access, "Test Min function");
   end Initialize;

   procedure Test_Max is
      Left  : constant m128 := (0.0, 1.0, -2.0, 1.0);
      Right : constant m128 := (0.0, 0.0, 1.0, 2.0);

      Expected : constant m128 := (0.0, 1.0, 1.0, 2.0);
      Result   : constant m128 := Max (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Max;

   procedure Test_Min is
      Left  : constant m128 := (0.0, 1.0, -2.0, 1.0);
      Right : constant m128 := (0.0, 0.0, 1.0, 2.0);

      Expected : constant m128 := (0.0, 0.0, -2.0, 1.0);
      Result   : constant m128 := Min (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Min;

end Test_SIMD_SSE_Math;
