--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Orka.SIMD.SSE2.Integers.Swizzle;

package body Orka.SIMD.SSE2.Integers.Arithmetic is

   function Sum (Elements : m128i) return Integer_32 is
      use SIMD.SSE2.Integers.Swizzle;

      --  From https://stackoverflow.com/a/35270026

      --  Elements:  X   Y   Z   W
      --  Shuffled:  Y   Y   W   W
      --            --------------- +
      --  Sum:      X+Y Y+Y Z+W W+W
      Shuffled : constant m128i := Duplicate_H (Elements);
      Sum      : constant m128i := Elements + Shuffled;

      --  Sum:      X+Y Y+Y Z+W W+W
      --  Permute:  Z+W  .   .   .
      --            --------------- +
      --  New sum:  X+Y+Z+W . . .

      Mask_2_0_0_0 : constant := 2 + 0 * 4 + 0 * 16 + 0 * 64;

      Result : constant m128i := Permute (Sum, Mask_2_0_0_0) + Sum;
   begin
      return Result (X);
   end Sum;

end Orka.SIMD.SSE2.Integers.Arithmetic;
