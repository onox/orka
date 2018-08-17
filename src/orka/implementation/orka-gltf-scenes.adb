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

with Orka.SIMD;

package body Orka.glTF.Scenes is

   function Create_Nodes (Nodes : Types.JSON_Array_Value) return Natural_Vectors.Vector is
      Result : Natural_Vectors.Vector;
   begin
      for Node of Nodes loop
         Result.Append (Natural (Long_Integer'(Node.Value)));
      end loop;
      return Result;
   end Create_Nodes;

   function Get_Matrix (Matrix : Types.JSON_Array_Value) return Transforms.Matrix4
     with Pre => Matrix.Length = 16;

   function Get_Matrix (Matrix : Types.JSON_Array_Value) return Transforms.Matrix4 is
      Result : Transforms.Matrix4;
   begin
      for I in Orka.SIMD.Index_Homogeneous loop
         for J in Orka.SIMD.Index_Homogeneous loop
            declare
               Column : constant Natural := Orka.SIMD.Index_Homogeneous'Pos (I) * 4;
               Row    : constant Natural := Orka.SIMD.Index_Homogeneous'Pos (J);
            begin
               Result (I) (J) := Matrix.Get (Column + Row + 1).Value;
            end;
         end loop;
      end loop;
      return Result;
   end Get_Matrix;

   function Get_Vector3 (Vector : Types.JSON_Array_Value) return Transforms.Vector4
     with Pre => Vector.Length = 3;

   function Get_Vector4 (Vector : Types.JSON_Array_Value) return Transforms.Vector4
     with Pre => Vector.Length = 4;

   function Get_Vector3 (Vector : Types.JSON_Array_Value) return Transforms.Vector4 is
     ((Vector.Get (1).Value, Vector.Get (2).Value, Vector.Get (3).Value, 0.0));

   function Get_Vector4 (Vector : Types.JSON_Array_Value) return Transforms.Vector4 is
     ((Vector.Get (1).Value, Vector.Get (2).Value, Vector.Get (3).Value, Vector.Get (4).Value));

   function Create_Node (Object : Types.JSON_Value'Class) return Node is
      Transform : constant Transform_Kind
        := (if Object.Contains ("matrix") then Matrix else TRS);
   begin
      return Result : Node (Transform) do
         Result.Name := Object.Get ("name").Value;
         Result.Children := Create_Nodes (Object.Get_Array_Or_Empty ("children"));
         Result.Mesh := Natural_Optional (Long_Integer'(Object.Get_Value_Or_Default ("mesh", Undefined).Value));

         case Transform is
            when Matrix =>
               Result.Matrix := Get_Matrix (Object.Get_Array ("matrix"));
            when TRS =>
               if Object.Contains ("translation") then
                  Result.Translation := Get_Vector3 (Object.Get_Array ("translation"));
               else
                  Result.Translation := (0.0, 0.0, 0.0, 0.0);
               end if;

               if Object.Contains ("Rotation") then
                  Result.Rotation := Get_Vector4 (Object.Get_Array_Or_Empty ("rotation"));
               else
                  Result.Rotation := (0.0, 0.0, 0.0, 1.0);
               end if;

               if Object.Contains ("Scale") then
                  Result.Scale := Get_Vector3 (Object.Get_Array_Or_Empty ("scale"));
               else
                  Result.Scale := (1.0, 1.0, 1.0, 0.0);
               end if;
         end case;
      end return;
   end Create_Node;

   function Get_Nodes
     (Nodes : Types.JSON_Array_Value) return Node_Vectors.Vector
   is
      Result : Node_Vectors.Vector;
   begin
      for Node of Nodes loop
         Result.Append (Create_Node (Node));
      end loop;
      return Result;
   end Get_Nodes;

   function Create_Scene
     (Object : Types.JSON_Value'Class) return Scene is
   begin
      return Result : Scene do
         Result.Name  := Object.Get ("name").Value;
         Result.Nodes := Create_Nodes (Object.Get_Array ("nodes"));
      end return;
   end Create_Scene;

   function Get_Scenes
     (Scenes : Types.JSON_Array_Value) return Scene_Vectors.Vector
   is
      Result : Scene_Vectors.Vector;
   begin
      for Scene of Scenes loop
         Result.Append (Create_Scene (Scene));
      end loop;
      return Result;
   end Get_Scenes;

end Orka.glTF.Scenes;
