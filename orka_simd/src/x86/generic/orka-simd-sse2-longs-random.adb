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

with Orka.SIMD.SSE2.Integers;
with Orka.SIMD.SSE2.Longs.Convert;

package body Orka.SIMD.SSE2.Longs.Random is

   package SSE2_Random renames Orka.SIMD.SSE2.Integers.Random;

   use SIMD.SSE2.Integers;

   function Convert is new Ada.Unchecked_Conversion (m128i, m128l);

   procedure Next (S : in out State; Value : out m128l) is
      V : m128i;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), V);
      Value := Convert (V);
   end Next;

   procedure Next (S : in out State; Value : out m128d) is
      Result : m128i;
   begin
      SSE2_Random.Next (SSE2_Random.State (S), Result);
      Value := SIMD.SSE2.Longs.Convert.To_Unit_Floats (Result);
   end Next;

   overriding
   procedure Reset (S : out State; Seed : Duration) is
   begin
      SSE2_Random.Reset (SSE2_Random.State (S), Seed);
   end Reset;

end Orka.SIMD.SSE2.Longs.Random;
