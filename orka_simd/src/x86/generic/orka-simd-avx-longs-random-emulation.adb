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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Doubles.Swizzle;
with Orka.SIMD.AVX.Integers.Swizzle;
with Orka.SIMD.SSE2.Doubles;
with Orka.SIMD.SSE2.Longs.Shift;
with Orka.SIMD.SSE2.Longs.Convert;

package body Orka.SIMD.AVX.Longs.Random.Emulation is

   package SSE2_Random renames Orka.SIMD.SSE2.Integers.Random;

   use SIMD.SSE2.Doubles;
   use SIMD.SSE2.Integers;
   use SIMD.SSE2.Longs;
   use SIMD.SSE2.Longs.Shift;
   use SIMD.AVX.Integers;
   use SIMD.AVX.Integers.Swizzle;
   use SIMD.AVX.Doubles.Swizzle;

   function Convert is new Ada.Unchecked_Conversion (m256i, m256l);

   procedure Next (S : in out State; Value : out m256l) is
      High, Low : m128i;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), Low);
      SSE2_Random.Next (SSE2_Random.State (S), High);
      Value := Convert (Pack (High => High, Low => Low));
   end Next;

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);
   function Convert is new Ada.Unchecked_Conversion (m128l, m128i);

   procedure Next (S : in out State; Value : out m256d) is
      Result : m128i;
      High, Low : m128d;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), Result);
      Low := SIMD.SSE2.Longs.Convert.To_Unit_Floats (Result);
      Result := Convert (Shift_Elements_Right_Zeros (Convert (Result)));
      High := SIMD.SSE2.Longs.Convert.To_Unit_Floats (Result);
      Value := Pack (High => High, Low => Low);
   end Next;

   overriding
   procedure Reset (S : out State; Seed : Duration) is
   begin
      SSE2_Random.Reset (SSE2_Random.State (S), Seed);
   end Reset;

end Orka.SIMD.AVX.Longs.Random.Emulation;
