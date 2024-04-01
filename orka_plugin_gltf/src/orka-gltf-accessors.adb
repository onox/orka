--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

package body Orka.glTF.Accessors is

   function Unsigned_Type (Value : Component_Kind) return GL.Types.Index_Type is
   begin
      case Value is
         when Unsigned_Short =>
            return GL.Types.UShort_Type;
         when Unsigned_Int =>
            return GL.Types.UInt_Type;
         when others =>
            --  Note: Unsigned_Byte is not supported for commonality with Vulkan
            raise Constraint_Error with "accessor.componentType is not ushort or uint";
      end case;
   end Unsigned_Type;

   function Create_Bounds (Bounds : Types.JSON_Value) return Transforms.Vector4 is
      Key : constant array (Positive range 1 .. 4) of Index_4D :=
        [1 => X, 2 => Y, 3 => Z, 4 => W];
   begin
      return Result : Transforms.Vector4 := Transforms.Zero_Vector do
         for Index in 1 .. Bounds.Length loop
            Result (Key (Index)) := Bounds.Get (Index).Value;
         end loop;
      end return;
   end Create_Bounds;

   function Create_Accessor
     (Object : Types.JSON_Value) return Accessor
   is
      View   : constant Long_Integer := Object.Get ("bufferView").Value;
      Offset : constant Long_Integer := Object.Get ("byteOffset", 0).Value;
      pragma Assert (Offset mod 4 = 0);

      Component_Type : constant Long_Integer := Object.Get ("componentType").Value;
      Element_Type   : constant String := Object.Get ("type").Value;

      Component : Component_Kind;
      Kind      : Attribute_Kind;
   begin
      case Component_Type is
         when 5120 =>
            Component := Byte;
         when 5121 =>
            Component := Unsigned_Byte;
         when 5122 =>
            Component := Short;
         when 5123 =>
            Component := Unsigned_Short;
         when 5125 =>
            Component := Unsigned_Int;
         when 5126 =>
            Component := Float;
         when others =>
            raise Constraint_Error with "Invalid accessor.componentType";
      end case;

      if Element_Type = "SCALAR" then
         Kind := Scalar;
      elsif Element_Type = "VEC2" then
         Kind := Vector2;
      elsif Element_Type = "VEC3" then
         Kind := Vector3;
      elsif Element_Type = "VEC4" then
         Kind := Vector4;
      elsif Element_Type = "MAT2" then
         Kind := Matrix2;
      elsif Element_Type = "MAT3" then
         Kind := Matrix3;
      elsif Element_Type = "MAT4" then
         Kind := Matrix4;
      else
         raise Constraint_Error with "Invalid accessor.type";
      end if;

      return Result : Accessor (Kind in Scalar .. Vector4) do
         Result.View       := Natural (View);
         Result.Offset     := Natural (Offset);
         Result.Component  := Component;
         Result.Kind       := Kind;
         Result.Normalized := Object.Get ("normalized", False).Value;
         Result.Count      := Positive (Long_Integer'(Object.Get ("count").Value));
         Result.Min_Bounds := Create_Bounds (Object.Get_Array_Or_Empty ("min"));
         Result.Max_Bounds := Create_Bounds (Object.Get_Array_Or_Empty ("max"));
      end return;
   end Create_Accessor;

   function Get_Accessors
     (Accessors : Types.JSON_Value) return Accessor_Vectors.Vector
   is
      Result : Accessor_Vectors.Vector (Capacity => Accessors.Length);
   begin
      for Accessor of Accessors loop
         --  TODO accessor.byteOffset + bufferView.byteOffset mod componentType = 0
         --
         --  TODO byteStride optional: if not defined, then values are tightly packed:
         --  = accessor.componentType * bytes(accessor.kind)
         --  bufferView.byteStride mod accessor.componentType = 0
         --
         --  TODO accessor.byteOffset + STRIDE * (accessor.count - 1)
         --    + SIZE_OF_ELEMENT <= bufferView.length

         if Accessor.Contains ("sparse") then
            raise Constraint_Error with "Sparse accessor is not supported";
         end if;

         Result.Append (Create_Accessor (Accessor));
      end loop;
      return Result;
   end Get_Accessors;

end Orka.glTF.Accessors;
