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

with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Streams.Stream_IO;
with Ada.Strings.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

with GL.Types;

with Orka.SIMD;
with Orka.glTF.Buffers;

package body Orka.Resources.Models.glTF is

   package Types is new JSON.Types (Long_Integer, Long_Float);

   use Types;
   use Orka.glTF.Buffers;

   procedure Set_Matrix (Scene : in out Trees.Tree; Node : String; Matrix : JSON_Array_Value)
     with Pre => Matrix.Length = 16;
   --  Uses the given matrix as the local transform of the node

   procedure Set_Matrix (Scene : in out Trees.Tree; Node : String; Matrix : JSON_Array_Value) is
      Local_Transform : Trees.Matrix4;
   begin
      for I in Orka.SIMD.Index_Homogeneous loop
         for J in Orka.SIMD.Index_Homogeneous loop
            declare
               Column : constant Natural := Orka.SIMD.Index_Homogeneous'Pos (I) * 4;
               Row    : constant Natural := Orka.SIMD.Index_Homogeneous'Pos (J);

               Value : constant Long_Float := Matrix.Get (Column + Row + 1).Value;
            begin
               Local_Transform (I) (J) := Trees.Transforms.Element_Type (Value);
            end;
         end loop;
      end loop;

      Scene.Set_Local_Transform (Scene.To_Cursor (Node), Local_Transform);
   end Set_Matrix;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   procedure Add_Scene_Nodes (Scene  : in out Trees.Tree;
                              Parts  : in out String_Maps.Map;
                              Nodes   : JSON_Value'Class;
                              Parents : JSON_Array_Value) is
      Current_Parents, Next_Parents : String_Vectors.Vector;
   begin
      --  Initialize Current_Parents with the elements in Parents
      for Parent of Parents loop
         Current_Parents.Append (Parent.Value);
      end loop;

      --  Add the children of Current_Parents, and then the
      --  children of those children, etc. etc.
      loop
         for Parent of Current_Parents loop
            declare
               Parent_Node : constant JSON_Value'Class := Nodes.Get (Parent);
            begin
               --  Use "matrix" for the local transform
               if Parent_Node.Contains ("matrix") then
                  Set_Matrix (Scene, Parent, Parent_Node.Get_Array ("matrix"));
               end if;
               --  TODO "rotate", "translate", "scale" not supported yet

               --  Add any children or meshes if there are no children
               declare
                  Children : constant JSON_Array_Value := Parent_Node.Get_Array_Or_Empty ("children");
               begin
                  declare
                     Meshes : constant JSON_Array_Value := Parent_Node.Get_Array_Or_Empty ("meshes");
                  begin
                     if Children.Length > 0 then
                        for Child of Children loop
                           Scene.Add_Node (Child.Value, Parent);
                           Next_Parents.Append (Child.Value);
                        end loop;
                     end if;

                     --  Add meshes as MDI parts
                     if Meshes.Length = 1 then
                        --  If there is one mesh, map Parent (a node) to the mesh name,
                        Parts.Insert (Parent, Meshes.Get (1).Value);
                     else
                        --  otherwise create a new node for each mesh name and then
                        --  map those nodes to their corresponding mesh names
                        for Mesh of Meshes loop
                           Scene.Add_Node (Mesh.Value, Parent);
                           Parts.Insert (Mesh.Value, Mesh.Value);
                        end loop;
                     end if;
                  end;
               end;
            end;
         end loop;

         exit when Next_Parents.Is_Empty;

         Current_Parents := Next_Parents;
         Next_Parents.Clear;
      end loop;
   end Add_Scene_Nodes;

   function Get_Buffers (Buffers : JSON_Object_Value) return Buffer_Maps.Map is
      Result : Buffer_Maps.Map;
   begin
      for Name of Buffers loop
         declare
            Buffer : constant JSON_Value'Class := Buffers.Get (Name);
         begin
            Ada.Text_IO.Put_Line ("Buffer length: " & Long_Integer'Image (Long_Integer'(Buffer.Get ("byteLength").Value)));
            Result.Insert (Name, Create_Buffer
              (URI    => Buffer.Get ("uri").Value,
               Length => Natural (Long_Integer'(Buffer.Get ("byteLength").Value))));
         end;
      end loop;
      return Result;
   end Get_Buffers;

   function Get_Buffer_Views
     (Buffers : Buffer_Maps.Map;
      Views   : JSON_Object_Value) return Buffer_View_Maps.Map
   is
      Result : Buffer_View_Maps.Map;
   begin
      for Name of Views loop
         declare
            View : constant JSON_Value'Class := Views.Get (Name);
         begin
            Result.Insert (Name, Create_Buffer_View
              (Buffer => Buffers.Element (String'(View.Get ("buffer").Value)).Data,
               Offset => Natural (Long_Integer'(View.Get ("byteOffset").Value)),
               Length => Natural (Long_Integer'(View.Get ("byteLength").Value)),
               Kind   => Target_Kinds (Integer (Long_Integer'(View.Get ("target").Value)))));
         end;
      end loop;
      return Result;
   end Get_Buffer_Views;

   procedure Add_Parts
     (Parts  : String_Maps.Map;
      Views  : Buffer_View_Maps.Map;
      Batch  : in out Buffers.MDI.Batch;
      Shapes : in out String_Vectors.Vector;
      Meshes, Accessors : JSON_Value'Class)
   is
      Instance_ID : Natural;

      use Ada.Streams;
      use GL.Types;
   begin
      for Part in Parts.Iterate loop
         declare
            Node_Name : constant String := String_Maps.Key (Part);
            Mesh_Name : constant String := String_Maps.Element (Part);
            pragma Assert (Meshes.Contains (Mesh_Name), "Mesh '" & Mesh_Name & "' not found");

            Primitives : constant JSON_Value'Class := Meshes.Get (Mesh_Name).Get_Array ("primitives");
            pragma Assert (Primitives.Length = 1, "Mesh '" & Mesh_Name & "' has more than one primitive");
            First_Primitive : constant JSON_Object_Value := Primitives.Get_Object (1);

            Accessor_Name : constant String := First_Primitive.Get ("attributes").Get ("POSITION").Value;
            View_Name     : constant String := Accessors.Get (Accessor_Name).Get ("bufferView").Value;

            --  TODO Assumes "indices" exists
            Accessor_Indices_Name : constant String := First_Primitive.Get ("indices").Value;
            View_Indices_Name     : constant String := Accessors.Get (Accessor_Indices_Name).Get ("bufferView").Value;
            pragma Assert (Accessors.Get (Accessor_Indices_Name).Get ("componentType").Value = 5125);

            Vertices_Bytes : Stream_Element_Array_Access := Elements (Views.Element (View_Name));
            Indices_Bytes  : Stream_Element_Array_Access := Elements (Views.Element (View_Indices_Name));

            subtype Vertices_Array is Single_Array (1 .. Int (Vertices_Bytes'Length / 4));
            subtype Indices_Array is UInt_Array (1 .. Int (Indices_Bytes'Length / 4));
            --  TODO Assumes here that each index is 4 bytes (only holds if unsigned int)
            --  TODO We have to look at "componentType" of the indices' accessor
            --  TODO Verify that indices' accessor's "byteOffset" = 0
            --       and "byteStride" = byte size of "componentType"

            function Convert_Vertices is new Ada.Unchecked_Conversion
              (Source => Stream_Element_Array, Target => Vertices_Array);
            function Convert_Indices is new Ada.Unchecked_Conversion
              (Source => Stream_Element_Array, Target => Indices_Array);
         begin
--            Ada.Text_IO.Put_Line ("Mesh " & Mesh_Name & " has POSITION accessor " & Accessor_Name);
--            Ada.Text_IO.Put_Line (Integer'Image (Vertices_Bytes'Length));
            --  TODO Making lots of assumptions here:
            --          - all accessors (POSITION, NORMAL, TEXCOORD_0) have same bufferView
            --          - attributes are interleaved and are VEC3, VEC3, VEC2
            --          - indices are unsigned int?

--            Ada.Text_IO.Put_Line ("Node: " & Node_Name);

            Batch.Append
              (new Single_Array'(Convert_Vertices (Vertices_Bytes.all)),
               new UInt_Array'(Convert_Indices (Indices_Bytes.all)),
               Instance_ID);
--            Ada.Text_IO.Put_Line ("Instance ID: " & Integer'Image (Instance_ID));
            Free_Stream_Array (Vertices_Bytes);
            Free_Stream_Array (Indices_Bytes);

            Shapes.Append (Node_Name);
         end;
      end loop;
      pragma Assert (Batch.Length = Natural (Parts.Length));
   end Add_Parts;

   function Load_Model (Path : String) return Model is
      File        : Ada.Streams.Stream_IO.File_Type;
      File_Stream : Ada.Streams.Stream_IO.Stream_Access;

      package Parsers is new JSON.Parsers (Types);

      type String_Access is access String;

      procedure Free_String is new Ada.Unchecked_Deallocation
        (Object => String, Name => String_Access);
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Path);

      declare
         File_Size : constant Integer := Integer (Ada.Streams.Stream_IO.Size (File));
         subtype File_String is String (1 .. File_Size);
         Raw_Contents : String_Access := new File_String;
      begin
         File_Stream := Ada.Streams.Stream_IO.Stream (File);
         File_String'Read (File_Stream, Raw_Contents.all);

         Ada.Streams.Stream_IO.Close (File);

         declare
            Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Raw_Contents);
            Object : constant JSON_Value'Class := Parsers.Parse (Stream);

            GL_Extensions       : constant JSON_Array_Value := Object.Get_Array_Or_Empty ("glExtensionsUsed");
            Required_Extensions : constant JSON_Array_Value := Object.Get_Array_Or_Empty ("extensionsRequired");
         begin
            Free_String (Raw_Contents);

            --  Require indices to be of type UInt
            if not (for some Extension of GL_Extensions => Extension.Value = "OES_element_index_uint") then
               raise Resource_Load_Error with "glTF asset '" & Path & "' does not use OES_element_index_uint";
            end if;

            --  Raise error if glTF asset requires (unsupported) KHR_binary_glTF extension
            if (for some Extension of Required_Extensions => Extension.Value = "KHR_binary_glTF") then
               raise Resource_Load_Error with "glTF asset '" & Path & "' requires (unsupported) KHR_binary_glTF";
            end if;

            declare
               --  TODO "nodes", "scenes", and "scene" are not required in .gltf file
               --  TODO only "meshes", "accessors", "asset", "buffers", and "bufferViews" are
               Scenes : constant JSON_Value'Class := Object.Get ("scenes");
               Nodes  : constant JSON_Value'Class := Object.Get ("nodes");

               Default_Scene : constant JSON_Value'Class := Scenes.Get (Object.Get ("scene").Value);
               Scene_Nodes   : constant JSON_Array_Value := Default_Scene.Get_Array ("nodes");
               pragma Assert (Scene_Nodes.Length >= 1);  --  "nodes" could be empty ([])

               Meshes    : constant JSON_Value'Class := Object.Get ("meshes");
               Accessors : constant JSON_Value'Class := Object.Get ("accessors");

               Parts  : String_Maps.Map;
               Shapes : String_Vectors.Vector;

               Mesh_Buffers : constant Buffer_Maps.Map := Get_Buffers (Object.Get_Object ("buffers"));
               Mesh_Views : constant Buffer_View_Maps.Map :=
                 Get_Buffer_Views (Mesh_Buffers, Object.Get_Object ("bufferViews"));

               Batch : Buffers.MDI.Batch := Buffers.MDI.Create_Batch (8);  --  3 + 3 + 2 = 8
            begin
               return Object : Model := (Scene => Trees.Create_Tree ("root"), others => <>) do
                  for Node of Scene_Nodes loop
                     Object.Scene.Add_Node (Node.Value, "root");
                  end loop;

                  Add_Scene_Nodes (Object.Scene, Parts, Nodes, Scene_Nodes);
                  Add_Parts (Parts, Mesh_Views, Batch, Shapes, Meshes, Accessors);

                  Create_Mesh (Object, Batch);
                  Batch.Clear;
                  Object.Shapes := Shapes;

                  --  TODO deallocate buffers in Mesh_Buffers
               end return;
            end;
         end;
      exception
         when others =>
            Free_String (Raw_Contents);
            raise;
      end;
   exception
      when others =>
         if Ada.Streams.Stream_IO.Is_Open (File) then
            Ada.Streams.Stream_IO.Close (File);
         end if;
         raise;
   end Load_Model;

end Orka.Resources.Models.glTF;
