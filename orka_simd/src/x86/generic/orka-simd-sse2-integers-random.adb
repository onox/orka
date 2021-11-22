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

with Orka.SIMD.SSE2.Integers.Arithmetic;
with Orka.SIMD.SSE2.Integers.Convert;
with Orka.SIMD.SSE2.Integers.Logical;
with Orka.SIMD.SSE2.Integers.Shift;

package body Orka.SIMD.SSE2.Integers.Random is

   --  This is an Ada port of xoshiro128++ random number generator.
   --
   --  https://arxiv.org/abs/1805.01407
   --  doi:10.1145/3460772
   --  https://arxiv.org/abs/1910.06437
   --
   --  David Blackman and Sebastiano Vigna.
   --  Scrambled linear pseudorandom number generators. ACM Trans. Math. Softw., 47:1−32, 2021.
   --
   --  The following comment is from https://prng.di.unimi.it/xoshiro128plusplus.c:
   --
   --  /*  Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)
   --
   --  To the extent possible under law, the author has dedicated all copyright
   --  and related and neighboring rights to this software to the public domain
   --  worldwide. This software is distributed without any warranty.
   --
   --  See <http://creativecommons.org/publicdomain/zero/1.0/>. */
   --
   --  /* This is xoshiro128++ 1.0, one of our 32-bit all-purpose, rock-solid
   --     generators. It has excellent speed, a state size (128 bits) that is
   --     large enough for mild parallelism, and it passes all tests we are aware
   --     of.
   --
   --     For generating just single-precision (i.e., 32-bit) floating-point
   --     numbers, xoshiro128+ is even faster.
   --
   --     The state must be seeded so that it is not everywhere zero. */
   procedure Next (S : in out State; Value : out m128i) is
      use Orka.SIMD.SSE2.Integers.Arithmetic;
      use Orka.SIMD.SSE2.Integers.Logical;
      use Orka.SIMD.SSE2.Integers.Shift;

      function Rotate_Left (X : m128i; K : Bits_Count) return m128i is
        (Shift_Bits_Left_Zeros (X, K) or Shift_Bits_Right_Zeros (X, Unsigned_32'Size - K));

      --  xoshiro128++ (xoshiro128+ is just S (0) + S (3))
      Result : constant m128i := Rotate_Left (S (0) + S (3), 7) + S (0);

      T : constant m128i := Shift_Bits_Left_Zeros (S (1), 9);
   begin
      S (2) := S (2) xor S (0);
      S (3) := S (3) xor S (1);
      S (1) := S (1) xor S (2);
      S (0) := S (0) xor S (3);

      S (2) := S (2) xor T;

      S (3) := Rotate_Left (S (3), 11);

      Value := Result;
   end Next;

   procedure Next (S : in out State; Value : out m128) is
      Result : m128i;
   begin
      Next (S, Result);
      Value := SIMD.SSE2.Integers.Convert.To_Unit_Floats (Result);
   end Next;

   procedure Reset (S : out State; Seed : Duration) is
      Value : constant Unsigned_32 := Unsigned_32 (Unsigned_64 (Seed) mod Unsigned_32'Modulus);

      function Convert is new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

      function Rotate_Left (X : Unsigned_32; K : Natural) return Unsigned_32 is
        ((X * 2**K) or (X / 2**(Unsigned_32'Size - K)));
   begin
      for I in S'Range loop
         S (I) :=
           (Convert (Rotate_Left (Value, 1)),
            Convert (Rotate_Left (Value, 2)),
            Convert (Rotate_Left (Value, 3)),
            Convert (Rotate_Left (Value, 4)));
      end loop;
   end Reset;

end Orka.SIMD.SSE2.Integers.Random;
