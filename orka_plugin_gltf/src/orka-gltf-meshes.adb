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

package body Orka.glTF.Meshes is

   function Attribute_Name_To_Kind (Name : String) return Attribute_Kind is
   begin
      if Name = "POSITION" then
         return Position;
      elsif Name = "NORMAL" then
         return Normal;
      elsif Name = "TEXCOORD_0" then
         return Texcoord_0;
      else
         raise Constraint_Error with "Invalid attribute";
      end if;
   end Attribute_Name_To_Kind;

   function Create_Primitive
     (Object : Types.JSON_Value) return Primitive
   is
      Attributes : constant Types.JSON_Value := Object.Get ("attributes");

      Indices  : constant Long_Integer := Object.Get ("indices", Undefined).Value;
      Material : constant Long_Integer := Object.Get ("material", Undefined).Value;
      Mode     : constant Long_Integer := Object.Get ("mode", 4).Value;
   begin
      --  TODO primitive.indices: When defined, the accessor must contain
      --  indices: the bufferView referenced by the accessor should have a
      --  target equal to 34963 (ELEMENT_ARRAY_BUFFER); componentType must
      --  be 5121 (UNSIGNED_BYTE), 5123 (UNSIGNED_SHORT) or 5125 (UNSIGNED_INT)
      return Result : Primitive do
         pragma Assert (Attributes.Length = 3);

         for Attribute of Attributes loop
            Result.Attributes (Attribute_Name_To_Kind (Attribute.Value)) :=
              Natural (Long_Integer'(Attributes.Get (Attribute.Value).Value));
         end loop;

         case Mode is
            when 0 =>
               Result.Mode := GL.Types.Points;
            when 1 =>
               Result.Mode := GL.Types.Lines;
            when 2 =>
               raise Constraint_Error with "Line_Loop is not supported";
            when 3 =>
               Result.Mode := GL.Types.Line_Strip;
            when 4 =>
               Result.Mode := GL.Types.Triangles;
            when 5 =>
               Result.Mode := GL.Types.Triangle_Strip;
            when 6 =>
               raise Constraint_Error with "Triangle_Fan is not supported";
            when others =>
               raise Constraint_Error with "Invalid primitive.mode";
         end case;

         Result.Indices  := Natural_Optional (Indices);
         Result.Material := Natural_Optional (Material);
      end return;
   end Create_Primitive;

   function Create_Primitives
     (Primitives : Types.JSON_Value) return Primitive is
   begin
      if Primitives.Length /= 1 then
         raise Constraint_Error with "Mesh without exactly one primitive is not supported";
      end if;
      return Create_Primitive (Primitives (1));
   end Create_Primitives;

   function Create_Mesh
     (Object : Types.JSON_Value) return Mesh is
   begin
      return Result : Mesh do
         Result.Primitives := Create_Primitives (Object.Get ("primitives"));
         Result.Name       := Name_Strings.To_Bounded_String (Object.Get ("name").Value);
      end return;
   end Create_Mesh;

   function Get_Meshes
     (Meshes : Types.JSON_Value) return Mesh_Vectors.Vector
   is
      Result : Mesh_Vectors.Vector (Capacity => Meshes.Length);
   begin
      for Mesh of Meshes loop
         Result.Append (Create_Mesh (Mesh));
      end loop;
      return Result;
   end Get_Meshes;

end Orka.glTF.Meshes;
