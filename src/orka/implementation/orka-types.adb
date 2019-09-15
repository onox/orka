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

with Orka.SIMD.AVX.Singles;
with Orka.SIMD.F16C;

package body Orka.Types is

   use SIMD.AVX.Singles;

   function Convert
     (Kind : Numeric_Type) return GL.Types.Numeric_Type
   is (case Kind is
      when UByte_Type  => GL.Types.UByte_Type,
      when UShort_Type => GL.Types.UShort_Type,
      when UInt_Type   => GL.Types.UInt_Type,

      when Byte_Type   => GL.Types.Byte_Type,
      when Short_Type  => GL.Types.Short_Type,
      when Int_Type    => GL.Types.Int_Type,

      when Half_Type   => GL.Types.Half_Type,
      when Single_Type => GL.Types.Single_Type,
      when Double_Type => GL.Types.Double_Type);

   function Convert
     (Kind : Index_Type) return GL.Types.Index_Type
   is (case Kind is
         when UShort_Type => GL.Types.UShort_Type,
         when UInt_Type   => GL.Types.UInt_Type);

   -----------------------------------------------------------------------------

   generic
      Size : GL.Types.Int;
      type Source_Type is private;
      type Target_Type is private;
      type Source_Array_Type is array (GL.Types.Size range <>) of Source_Type;
      type Target_Array_Type is array (GL.Types.Size range <>) of Target_Type;
      with function Convert_Slice (Values : Source_Array_Type) return Target_Array_Type;
   function Generic_Convert (Elements : Source_Array_Type) return Target_Array_Type;

   function Generic_Convert (Elements : Source_Array_Type) return Target_Array_Type is
      use type GL.Types.Int;

      Iterations : constant GL.Types.Size := Elements'Length / Size;
      Remainder  : constant GL.Types.Size := Elements'Length rem Size;

      Result : Target_Array_Type (Elements'Range);
      Offset : GL.Types.Size := Elements'First;
   begin
      --  Convert Size elements in each iteration
      for Index in 0 .. Iterations - 1 loop
         Offset := Elements'First + Index * Size;
         Result (Offset .. Offset + Size - 1) :=
           Convert_Slice (Elements (Offset .. Offset + Size - 1));
      end loop;
      pragma Assert (Elements'Last - Remainder = Offset + Size - 1);

      --  Convert remaining elements
      Result (Elements'Last - Remainder + 1 .. Elements'Last) :=
        Convert_Slice (Elements (Elements'Last - Remainder + 1 .. Elements'Last));
      return Result;
   end Generic_Convert;

   -----------------------------------------------------------------------------

   function Convert_Slice (Values : GL.Types.Single_Array) return GL.Types.Half_Array is
      use SIMD.F16C;

      S : GL.Types.Single_Array (1 .. m256'Length);
      H : m128i;
   begin
      S (1 .. Values'Length) := Values;
      H := Convert_Nearest_Integer (m256 (S));
      return GL.Types.Half_Array (H) (1 .. Values'Length);
   end Convert_Slice;

   function Convert_Slice (Values : GL.Types.Half_Array) return GL.Types.Single_Array is
      use SIMD.F16C;

      S : GL.Types.Half_Array (1 .. m256'Length);
      H : m256;
   begin
      S (1 .. Values'Length) := Values;
      H := Convert (m128i (S));
      return GL.Types.Single_Array (H) (1 .. Values'Length);
   end Convert_Slice;

   function Convert_Single is new Generic_Convert
     (m256'Length, GL.Types.Single, GL.Types.Half,
      GL.Types.Single_Array, GL.Types.Half_Array, Convert_Slice);

   function Convert_Half is new Generic_Convert
     (m256'Length, GL.Types.Half, GL.Types.Single,
      GL.Types.Half_Array, GL.Types.Single_Array, Convert_Slice);

   function Convert
     (Elements : GL.Types.Single_Array) return GL.Types.Half_Array renames Convert_Single;
   function Convert
     (Elements : GL.Types.Half_Array) return GL.Types.Single_Array renames Convert_Half;

   -----------------------------------------------------------------------------

   function Clamp (Value : in Source) return Target is
      A : constant Source := Source'Min (Source (Target'Last), Value);
      B : constant Source := Source'Max (Source (Target'First), A);
   begin
      return Target (B);
   end Clamp;

   function Normalize_Periodic (Value : in Source) return Target is
      Target_Min   : constant Source := Source (Target'First);
      Target_Range : constant Source := Source (Target'Last - Target'First);
   begin
      return Target (Value - Target_Range * Source'Floor ((Value - Target_Min) / Target_Range));
   end Normalize_Periodic;

end Orka.Types;
