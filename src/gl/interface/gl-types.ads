--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with GL.Algebra;

package GL.Types is
   pragma Pure;

   --  These are the types you can and should use with OpenGL functions
   --  (particularly when dealing with buffer objects).
   --  Types that are only used internally, but may be needed when interfacing
   --  with OpenGL-related library APIs can be found in GL.Low_Level.

   type long_long_int is range -(2 ** 63) .. +(2 ** 63 - 1);
   --  Based on C99 long long int

   type unsigned_long_long is mod 2 ** 64;
   --  Based on C99 unsigned long long int

   subtype UInt64 is unsigned_long_long;

   --  Signed integer types
   type Byte  is new C.signed_char;
   type Short is new C.short;
   type Int   is new C.int;
   type Long  is new long_long_int;

   subtype Size is Int range 0 .. Int'Last;
   subtype Long_Size is Long range 0 .. Long'Last;

   subtype Positive_Size is Size range 1 .. Size'Last;

   --  Unsigned integer types
   type UByte  is new C.unsigned_char;
   type UShort is new C.unsigned_short;
   type UInt   is new C.unsigned;

   --  Floating point types ("Single" is used to avoid conflicts with Float)
   subtype Half is Short;  --  F16C extension can be used to convert from/to Single
   type Single is new C.C_float;
   type Double is new C.double;

   subtype Normalized_Single is Single range 0.0 .. 1.0;

   --  Array types
   type Byte_Array   is array (Size range <>) of aliased Byte;
   type Short_Array  is array (Size range <>) of aliased Short;
   type Int_Array    is array (Size range <>) of aliased Int;

   type UByte_Array  is array (Size range <>) of aliased UByte;
   type UShort_Array is array (Size range <>) of aliased UShort;
   type UInt_Array   is array (Size range <>) of aliased UInt;

   type Half_Array   is array (Size range <>) of aliased Half;
   type Single_Array is array (Size range <>) of aliased Single;
   type Double_Array is array (Size range <>) of aliased Double;

   type Size_Array is array (Size range <>) of aliased Size;

   pragma Convention (C, Byte_Array);
   pragma Convention (C, Short_Array);
   pragma Convention (C, Int_Array);

   pragma Convention (C, UByte_Array);
   pragma Convention (C, UShort_Array);
   pragma Convention (C, UInt_Array);

   pragma Convention (C, Half_Array);
   pragma Convention (C, Single_Array);
   pragma Convention (C, Double_Array);

   pragma Convention (C, Size_Array);

   --  Type descriptors
   type Numeric_Type is (Byte_Type, UByte_Type, Short_Type,
                         UShort_Type, Int_Type, UInt_Type,
                         Single_Type, Double_Type, Half_Type);

   type Index_Type is (UShort_Type, UInt_Type);

   type Connection_Mode is (Points, Lines, Line_Loop, Line_Strip, Triangles,
                            Triangle_Strip, Triangle_Fan, Lines_Adjacency,
                            Line_Strip_Adjacency, Triangles_Adjacency,
                            Triangle_Strip_Adjacency, Patches);

   type Compare_Function is (Never, Less, Equal, LEqual, Greater, Not_Equal,
                             GEqual, Always);

   subtype Component_Count is Int range 1 .. 4;
   --  Counts the number of components for vertex attributes

   subtype Byte_Count is Int range 1 .. 4
     with Static_Predicate => Byte_Count in 1 | 2 | 4;
   --  Number of bytes of a component

   package Bytes is new GL.Algebra (Element_Type => Byte, Index_Type => Size);

   package Shorts is new GL.Algebra (Element_Type => Short, Index_Type => Size);

   package Ints is new GL.Algebra (Element_Type => Int, Index_Type => Size);

   package Longs is new GL.Algebra (Element_Type => Long, Index_Type => Size);

   package UBytes is new GL.Algebra (Element_Type => UByte, Index_Type => Size);

   package UShorts is new GL.Algebra (Element_Type => UShort, Index_Type => Size);

   package UInts is new GL.Algebra (Element_Type => UInt, Index_Type => Size);

   package Singles is new GL.Algebra (Element_Type => Single, Index_Type => Size);

   package Doubles is new GL.Algebra (Element_Type => Double, Index_Type => Size);

private

   for Numeric_Type use (Byte_Type   => 16#1400#,
                         UByte_Type  => 16#1401#,
                         Short_Type  => 16#1402#,
                         UShort_Type => 16#1403#,
                         Int_Type    => 16#1404#,
                         UInt_Type   => 16#1405#,
                         Single_Type => 16#1406#,
                         Double_Type => 16#140A#,
                         Half_Type   => 16#140B#);
   for Numeric_Type'Size use UInt'Size;

   for Index_Type use (UShort_Type => 16#1403#,
                       UInt_Type   => 16#1405#);
   for Index_Type'Size use UInt'Size;

   for Connection_Mode use (Points                   => 16#0000#,
                            Lines                    => 16#0001#,
                            Line_Loop                => 16#0002#,
                            Line_Strip               => 16#0003#,
                            Triangles                => 16#0004#,
                            Triangle_Strip           => 16#0005#,
                            Triangle_Fan             => 16#0006#,
                            Lines_Adjacency          => 16#000A#,
                            Line_Strip_Adjacency     => 16#000B#,
                            Triangles_Adjacency      => 16#000C#,
                            Triangle_Strip_Adjacency => 16#000D#,
                            Patches                  => 16#000E#);
   for Connection_Mode'Size use UInt'Size;

   for Compare_Function use (Never     => 16#0200#,
                             Less      => 16#0201#,
                             Equal     => 16#0202#,
                             LEqual    => 16#0203#,
                             Greater   => 16#0204#,
                             Not_Equal => 16#0205#,
                             GEqual    => 16#0206#,
                             Always    => 16#0207#);
   for Compare_Function'Size use UInt'Size;

end GL.Types;
