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

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

with Orka.SIMD.AVX.Integers.Shift.Emulation;

package body Test_SIMD_AVX_Shift_Emulation is

   use Orka;
   use Orka.SIMD.AVX.Integers;
   use Orka.SIMD.AVX.Integers.Shift.Emulation;

   use AUnit.Assertions;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Shift_Left_Zeros (Object : in out Test) is
      Values : constant m256i := [0, 1, 2, 4, 8, 2**8, 2**16, 2**30];

      --  x86 is little endian, so 'left' element is on the right side
      Expected : constant m256i := [0, 0, 1, 2, 4, 8, 2**8, 2**16];
      Result   : constant m256i := Shift_Elements_Left_Zeros (Values);
   begin
      for I in Result'Range loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_Shift_Left_Zeros;

   procedure Test_Shift_Right_Zeros (Object : in out Test) is
      Values : constant m256i := [0, 1, 2, 4, 8, 2**8, 2**16, 2**30];

      --  x86 is little endian, so 'left' element is on the right side
      Expected : constant m256i := [1, 2, 4, 8, 2**8, 2**16, 2**30, 0];
      Result   : constant m256i := Shift_Elements_Right_Zeros (Values);
   begin
      for I in Result'Range loop
         Assert (Expected (I) = Result (I), "Unexpected Integer at " & I'Image);
      end loop;
   end Test_Shift_Right_Zeros;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - AVX - Integers - Shift (emulation)) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Shift_Elements_Left_Zeros", Test_Shift_Left_Zeros'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function Shift_Elements_Right_Zeros", Test_Shift_Right_Zeros'Access));

      return Test_Suite'Access;
   end Suite;

end Test_SIMD_AVX_Shift_Emulation;
