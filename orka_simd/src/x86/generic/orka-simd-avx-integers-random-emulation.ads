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

with Orka.SIMD.AVX.Singles;

private with Orka.SIMD.SSE2.Integers.Random;

package Orka.SIMD.AVX.Integers.Random.Emulation
  with SPARK_Mode => On
is
   pragma Pure;

   use Orka.SIMD.AVX.Singles;

   type State is limited private;
   pragma Preelaborable_Initialization (State);

   procedure Next (S : in out State; Value : out m256i)
     with Inline_Always,
          Global  => null,
          Depends => ((S, Value) => S);
   --  Use and modify the given state to generate multiple
   --  random unsigned integers

   procedure Next (S : in out State; Value : out m256)
     with Inline_Always,
          Global  => null,
          Depends => ((S, Value) => S);
   --  Use and modify the given state to generate multiple random
   --  floating-point numbers in the interval [0, 1).

   procedure Reset (S : out State; Seed : Duration)
     with Global  => null,
          Depends => (S => Seed),
          Pre     => Seed /= 0.0;

private

   type State is new Orka.SIMD.SSE2.Integers.Random.State;

end Orka.SIMD.AVX.Integers.Random.Emulation;
