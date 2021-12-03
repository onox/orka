--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Orka.SIMD.AVX.Doubles;

package Orka.SIMD.AVX2.Longs.Random
  with SPARK_Mode => On
is
   pragma Pure;

   use Orka.SIMD.AVX.Doubles;

   type State is limited private;
   pragma Preelaborable_Initialization (State);

   procedure Next (S : in out State; Value : out m256l)
     with Inline_Always,
          Global  => null,
          Depends => ((S, Value) => S);
   --  Use and modify the given state to generate multiple
   --  random unsigned integers

   procedure Next (S : in out State; Value : out m256d)
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

   type State is array (Integer_64 range 0 .. 3) of m256l;

end Orka.SIMD.AVX2.Longs.Random;
