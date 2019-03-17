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

with Ada.Containers.Indefinite_Vectors;

with GL.Types;

with Orka.Transforms.Singles.Vectors;

package Orka.glTF.Accessors is
   pragma Preelaborate;

   subtype Stride_Natural is Natural range Natural'First .. 255;

   type Component_Kind is (Byte, Unsigned_Byte, Short, Unsigned_Short, Unsigned_Int, Float);

   type Attribute_Kind is (Scalar, Vector2, Vector3, Vector4, Matrix2, Matrix3, Matrix4);

   Bytes_Element : constant array (Component_Kind) of Positive :=
     (Byte           => 1,
      Unsigned_Byte  => 1,
      Short          => 2,
      Unsigned_Short => 2,
      Unsigned_Int   => 4,
      Float          => 4);

   Attribute_Length : constant array (Attribute_Kind) of Positive :=
     (Scalar  => 1,
      Vector2 => 2,
      Vector3 => 3,
      Vector4 => 4,
      Matrix2 => 4,
      Matrix3 => 9,
      Matrix4 => 16);

   Numeric_Type : constant array (Component_Kind) of GL.Types.Numeric_Type :=
     (Byte           => GL.Types.Byte_Type,
      Unsigned_Byte  => GL.Types.UByte_Type,
      Short          => GL.Types.Short_Type,
      Unsigned_Short => GL.Types.UShort_Type,
      Unsigned_Int   => GL.Types.UInt_Type,
      Float          => GL.Types.Single_Type);

   function Unsigned_Type (Value : Component_Kind) return GL.Types.Index_Type;

   package Transforms renames Orka.Transforms.Singles.Vectors;

   type Accessor (Bounds : Boolean) is record
      View       : Natural;
      Offset     : Natural;
      Component  : Component_Kind;
      Normalized : Boolean;
      Count      : Positive;
      Kind       : Attribute_Kind;
      case Bounds is
         when True =>
            Min_Bounds : Transforms.Vector4;
            Max_Bounds : Transforms.Vector4;
         when False =>
            null;
      end case;
   end record;

   package Accessor_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Accessor);

   function Get_Accessors
     (Accessors : Types.JSON_Value) return Accessor_Vectors.Vector;

end Orka.glTF.Accessors;
