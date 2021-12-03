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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX2.Longs.Arithmetic;
with Orka.SIMD.AVX2.Longs.Convert;
with Orka.SIMD.AVX2.Longs.Logical;
with Orka.SIMD.AVX2.Longs.Shift;

package body Orka.SIMD.AVX2.Longs.Random is

   --  This is an Ada port of xoshiro256++ random number generator.
   --
   --  https://arxiv.org/abs/1805.01407
   --  doi:10.1145/3460772
   --  https://arxiv.org/abs/1910.06437
   --
   --  David Blackman and Sebastiano Vigna.
   --  Scrambled linear pseudorandom number generators. ACM Trans. Math. Softw., 47:1−32, 2021.
   --
   --  The following comment is from https://prng.di.unimi.it/xoshiro256plusplus.c:
   --
   --  /*  Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)
   --
   --  To the extent possible under law, the author has dedicated all copyright
   --  and related and neighboring rights to this software to the public domain
   --  worldwide. This software is distributed without any warranty.
   --
   --  See <http://creativecommons.org/publicdomain/zero/1.0/>. */
   --
   --  /* This is xoshiro256++ 1.0, one of our all-purpose, rock-solid generators.
   --     It has excellent (sub-ns) speed, a state (256 bits) that is large
   --     enough for any parallel application, and it passes all tests we are
   --     aware of.

   --     For generating just floating-point numbers, xoshiro256+ is even faster.

   --     The state must be seeded so that it is not everywhere zero. If you have
   --     a 64-bit seed, we suggest to seed a splitmix64 generator and use its
   --     output to fill s. */
   procedure Next (S : in out State; Value : out m256l) is
      use Orka.SIMD.AVX2.Longs.Arithmetic;
      use Orka.SIMD.AVX2.Longs.Logical;
      use Orka.SIMD.AVX2.Longs.Shift;

      function Rotate_Left (X : m256l; K : Bits_Count) return m256l is
        (Shift_Bits_Left_Zeros (X, K) or Shift_Bits_Right_Zeros (X, Unsigned_64'Size - K));

      --  xoshiro256++ (xoshiro256+ is just S (0) + S (3))
      Result : constant m256l := Rotate_Left (S (0) + S (3), 23) + S (0);

      T : constant m256l := Shift_Bits_Left_Zeros (S (1), 17);
   begin
      S (2) := S (2) xor S (0);
      S (3) := S (3) xor S (1);
      S (1) := S (1) xor S (2);
      S (0) := S (0) xor S (3);

      S (2) := S (2) xor T;

      S (3) := Rotate_Left (S (3), 45);

      Value := Result;
   end Next;

   procedure Next (S : in out State; Value : out m256d) is
      Result : m256l;
   begin
      Next (S, Result);
      Value := SIMD.AVX2.Longs.Convert.To_Unit_Floats (Result);
   end Next;

   procedure Reset (S : out State; Seed : Duration) is
      Value : constant Unsigned_64 := Unsigned_64 (Seed);

      function Convert is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

      function Rotate_Left (X : Unsigned_64; K : Natural) return Unsigned_64 is
        ((X * 2**K) or (X / 2**(Unsigned_64'Size - K)));
   begin
      for I in S'Range loop
         S (I) :=
           (Convert (Rotate_Left (Value, 1)),
            Convert (Rotate_Left (Value, 2)),
            Convert (Rotate_Left (Value, 3)),
            Convert (Rotate_Left (Value, 4)));
      end loop;
   end Reset;

end Orka.SIMD.AVX2.Longs.Random;
