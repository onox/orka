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

with Orka.SIMD.AVX.Integers.Swizzle;
with Orka.SIMD.AVX.Singles.Swizzle;
with Orka.SIMD.SSE.Singles;
with Orka.SIMD.SSE2.Integers;

package body Orka.SIMD.AVX.Integers.Random.Emulation is

   package SSE2_Random renames Orka.SIMD.SSE2.Integers.Random;

   use SIMD.SSE.Singles;
   use SIMD.SSE2.Integers;
   use SIMD.AVX.Integers.Swizzle;
   use SIMD.AVX.Singles.Swizzle;

   procedure Next (S : in out State; Value : out m256i) is
      High, Low : m128i;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), Low);
      SSE2_Random.Next (SSE2_Random.State (S), High);
      Value := Pack (High => High, Low => Low);
   end Next;

   procedure Next (S : in out State; Value : out m256) is
      High, Low : m128;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), Low);
      SSE2_Random.Next (SSE2_Random.State (S), High);
      Value := Pack (High => High, Low => Low);
   end Next;

   overriding
   procedure Reset (S : out State; Seed : Duration) is
   begin
      SSE2_Random.Reset (SSE2_Random.State (S), Seed);
   end Reset;

end Orka.SIMD.AVX.Integers.Random.Emulation;
