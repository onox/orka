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

with GL.Types;

with Orka.Algebra;
with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Doubles.Matrices;

package Orka.Types is
   pragma Preelaborate;

   type Element_Type is
      --  Numeric types
     (UByte_Type,
      UShort_Type,
      UInt_Type,
      Byte_Type,
      Short_Type,
      Int_Type,
      Half_Type,
      Single_Type,
      Double_Type,
      --  Composite types
      Single_Vector_Type,
      Double_Vector_Type,
      Single_Matrix_Type,
      Double_Matrix_Type,
      Arrays_Command_Type,
      Elements_Command_Type,
      Dispatch_Command_Type);

   subtype Numeric_Type is Element_Type range UByte_Type .. Double_Type;

   subtype Signed_Numeric_Type is Numeric_Type range Byte_Type .. Double_Type;

   subtype Unsigned_Numeric_Type is Numeric_Type range UByte_Type .. UInt_Type;

   subtype Composite_Type is Element_Type range Single_Vector_Type .. Dispatch_Command_Type;

   function Convert
     (Kind : Types.Numeric_Type) return GL.Types.Numeric_Type;

   function Convert
     (Kind : Types.Unsigned_Numeric_Type) return GL.Types.Unsigned_Numeric_Type;

   -----------------------------------------------------------------------------

   package Singles is new Orka.Algebra (Orka.Transforms.Singles.Matrices);

   package Doubles is new Orka.Algebra (Orka.Transforms.Doubles.Matrices);

   function Convert (Elements : GL.Types.Single_Array) return GL.Types.Half_Array
     with Post => Elements'Length = Convert'Result'Length;
   function Convert (Elements : GL.Types.Half_Array) return GL.Types.Single_Array
     with Post => Elements'Length = Convert'Result'Length;

   -----------------------------------------------------------------------------

   generic
      type Source is digits <>;
      type Target is digits <>;
   function Clamp (Value : in Source) return Target;

   generic
      type Source is digits <>;
      type Target is digits <>;
   function Normalize_Periodic (Value : in Source) return Target;

end Orka.Types;
