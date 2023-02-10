--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package Orka is
   pragma Pure;

   type Index_4D is (X, Y, Z, W);

   subtype Index_2D is Index_4D range X .. Y;
   subtype Index_3D is Index_4D range X .. Z;

   ----------------------------------------------------------------------------

   --  GL types must have an exact number of bits and may not be equal
   --  to a similar type in Interfaces.C.
   --
   --  See Table 2.2 of the OpenGL specification.

   type Integer_8  is range -(2 **  7) .. +(2 ** 7 - 1)  with Size =>  8;

   type Integer_16 is range -(2 ** 15) .. +(2 ** 15 - 1) with Size => 16;
   type Integer_32 is range -(2 ** 31) .. +(2 ** 31 - 1) with Size => 32;
   type Integer_64 is range -(2 ** 63) .. +(2 ** 63 - 1) with Size => 64;

   subtype Size is Integer_32 range 0 .. Integer_32'Last;

   type Size_3D is array (Index_3D) of Size;

   type Integer_8_Array  is array (Size range <>) of aliased Integer_8  with Convention => C;
   type Integer_16_Array is array (Size range <>) of aliased Integer_16 with Convention => C;
   type Integer_32_Array is array (Size range <>) of aliased Integer_32 with Convention => C;

   ----------------------------------------------------------------------------

   type Unsigned_8  is mod 2 ** 8  with Size => 8;
   type Unsigned_16 is mod 2 ** 16 with Size => 16;
   type Unsigned_32 is mod 2 ** 32 with Size => 32;
   type Unsigned_64 is mod 2 ** 64 with Size => 64;

   type Unsigned_8_Array  is array (Size range <>) of aliased Unsigned_8  with Convention => C;
   type Unsigned_16_Array is array (Size range <>) of aliased Unsigned_16 with Convention => C;
   type Unsigned_32_Array is array (Size range <>) of aliased Unsigned_32 with Convention => C;

   ----------------------------------------------------------------------------

   subtype Float_16 is Integer_16;
   --  F16C extension can be used to convert from/to Float_32

   type Float_32 is digits  6 range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
   type Float_64 is digits 15 range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;

   overriding
   function "=" (Left, Right : Float_32) return Boolean;

   overriding
   function "=" (Left, Right : Float_64) return Boolean;

   type Float_16_Array is array (Size range <>) of aliased Float_16 with Convention => C;
   type Float_32_Array is array (Size range <>) of aliased Float_32 with Convention => C;
   type Float_64_Array is array (Size range <>) of aliased Float_64 with Convention => C;

   ----------------------------------------------------------------------------

   type Time is private;

   function "-" (Left, Right : Time) return Duration;
   function "-" (Left : Time; Right : Duration) return Time;

private

   overriding
   function "=" (Left, Right : Float_32) return Boolean is
     (abs (Left - Right) <= Float_32'Model_Epsilon);

   overriding
   function "=" (Left, Right : Float_64) return Boolean is
     (abs (Left - Right) <= Float_64'Model_Epsilon);

   type Time is new Duration;

   function "-" (Left, Right : Time) return Duration is (Duration (Left) - Duration (Right));

   function "-" (Left : Time; Right : Duration) return Time is (Time (Duration (Left) - Right));

end Orka;
