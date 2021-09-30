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

with Orka.SIMD.SSE.Singles.Arithmetic;
with Orka.SIMD.SSE.Singles.Swizzle;
with Orka.SIMD.SSE3.Singles.Swizzle;

package body Orka.SIMD.SSE3.Singles.Arithmetic is

   function Sum (Elements : m128) return Float_32 is
      use SIMD.SSE.Singles.Arithmetic;
      use SIMD.SSE.Singles.Swizzle;
      use SIMD.SSE3.Singles.Swizzle;

      --  From https://stackoverflow.com/a/35270026

      --  Elements:  X   Y   Z   W
      --  Shuffled:  Y   Y   W   W
      --            --------------- +
      --  Sum:      X+Y Y+Y Z+W W+W
      Shuffled : constant m128 := Duplicate_H (Elements);
      Sum      : constant m128 := Elements + Shuffled;

      --  Sum:      X+Y Y+Y Z+W W+W
      --  Move:     Z+W W+W  W   W
      --            --------------- +
      --  New sum:  X+Y+Z+W . . .
      Result : constant m128 := Move_HL (Shuffled, Sum) + Sum;
   begin
      return Result (X);
   end Sum;

end Orka.SIMD.SSE3.Singles.Arithmetic;
