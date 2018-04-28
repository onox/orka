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

with System;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with JSON.Parsers;
with JSON.Streams;

with GL.Objects.Buffers;
with GL.Pixels;
with GL.Types.Indirect;

with Orka.Types;

package body Orka.Resources.Models.glTF is

   Default_Root_Name : constant String := "root";

   --   package Debug_Messages is new GL.Debug.Messages (GL.Debug.Third_Party, GL.Debug.Other);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   procedure Add_Nodes
     (Scene   : in out Trees.Tree;
      Parts   : in out String_Maps.Map;
      Nodes   : Orka.glTF.Scenes.Node_Vectors.Vector;
      Parents : Orka.glTF.Scenes.Natural_Vectors.Vector)
   is
      Current_Parents, Next_Parents : Orka.glTF.Scenes.Natural_Vectors.Vector;
   begin
      --  Initialize Current_Parents with the elements in Parents
      for Parent_Index of Parents loop
         Current_Parents.Append (Parent_Index);
      end loop;

      --  Add the children of Current_Parents, and then the
      --  children of those children, etc. etc.
      loop
         for Parent_Index of Current_Parents loop
            declare
               Parent_Node : Orka.glTF.Scenes.Node renames Nodes (Parent_Index);
               Parent_Name : constant String := Parent_Node.Name.all;

               use type Orka.glTF.Scenes.Transform_Kind;
            begin
               --  Use "matrix" or TRS for the local transform
               if Parent_Node.Transform = Orka.glTF.Scenes.Matrix then
                  Scene.Set_Local_Transform (Scene.To_Cursor (Parent_Name), Parent_Node.Matrix);
               else
                  declare
                     Local_Transform : Trees.Matrix4 := Transforms.Identity_Value;
                  begin
                     Transforms.Scale (Local_Transform, Parent_Node.Scale);
                     Transforms.Rotate_At_Origin (Local_Transform, Parent_Node.Rotation);
                     Transforms.Translate (Local_Transform, Parent_Node.Translation);

                     Scene.Set_Local_Transform (Scene.To_Cursor (Parent_Name), Local_Transform);
                  end;
               end if;

               --  Add the children to the scene as nodes
               for Child_Index of Parent_Node.Children loop
                  Scene.Add_Node (Nodes (Child_Index).Name.all, Parent_Name);
                  Next_Parents.Append (Child_Index);
               end loop;

               --  Add mesh as MDI part by mapping the parent (a node) to the mesh index
               if Parent_Node.Mesh /= Orka.glTF.Undefined then
                  Parts.Insert (Parent_Name, Parent_Node.Mesh);
               end if;
            end;
         end loop;

         exit when Next_Parents.Is_Empty;

         Current_Parents := Next_Parents;
         Next_Parents.Clear;
      end loop;
   end Add_Nodes;

   function Shape_List
     (Parts : String_Maps.Map;
      Scene : Trees.Tree) return Cursor_Array_Holder.Holder
   is
      Shapes : Cursor_Array (1 .. Positive (Parts.Length));

      procedure Set_Name (Position : String_Maps.Cursor) is
         Mesh_Index : Natural renames String_Maps.Element (Position);
         Node_Name  : String  renames String_Maps.Key (Position);
      begin
         Shapes (Mesh_Index + 1) := Scene.To_Cursor (Node_Name);
      end Set_Name;
   begin
      Parts.Iterate (Set_Name'Access);
      return Cursor_Array_Holder.To_Holder (Shapes);
   end Shape_List;

   function Bounds_List
     (Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : Orka.glTF.Meshes.Mesh_Vectors.Vector) return Orka.Types.Singles.Vector4_Array
   is
      use type GL.Types.Size;
      use Orka.glTF.Accessors;

      Mesh_Index : GL.Types.Size := 1;
   begin
      return Result : Orka.Types.Singles.Vector4_Array (1 .. GL.Types.Size (Meshes.Length) * 2) do
         for Mesh of Meshes loop
            declare
               Primitives : Orka.glTF.Meshes.Primitive_Vectors.Vector renames Mesh.Primitives;
               First_Primitive : Orka.glTF.Meshes.Primitive renames Primitives (0);

               Attribute_Position : constant Natural := First_Primitive.Attributes ("POSITION");
               Accessor_Position  : Accessor renames Accessors (Attribute_Position);

               pragma Assert (Accessor_Position.Bounds);
            begin
               Result (Mesh_Index + 0) := Accessor_Position.Min_Bounds;
               Result (Mesh_Index + 1) := Accessor_Position.Max_Bounds;
               Mesh_Index := Mesh_Index + 2;
            end;
         end loop;
      end return;
   end Bounds_List;

   generic
      type Target_Type is private;
      type Target_Array is array (GL.Types.Size range <>) of aliased Target_Type;
      type Target_Array_Access is access Target_Array;
   package Buffer_View_Conversions is

      generic
         type Source_Type is private;
         type Source_Array is array (GL.Types.Size range <>) of aliased Source_Type;
         with function Cast (Value : Source_Type) return Target_Type;
      function Convert_Array (Elements : Source_Array) return Target_Array;

      generic
         type Source_Type is private;
         type Source_Array is array (GL.Types.Size range <>) of aliased Source_Type;
         with function Convert_Array (Elements : Source_Array) return Target_Array;
      procedure Get_Array
        (Accessor : Orka.glTF.Accessors.Accessor;
         View     : Orka.glTF.Buffers.Buffer_View;
         Target   : not null Target_Array_Access);

   end Buffer_View_Conversions;

   package body Buffer_View_Conversions is

      function Convert_Array (Elements : Source_Array) return Target_Array is
      begin
         return Result : Target_Array (Elements'Range) do
            for Index in Elements'Range loop
               Result (Index) := Cast (Elements (Index));
            end loop;
         end return;
      end Convert_Array;

      procedure Get_Array
        (Accessor : Orka.glTF.Accessors.Accessor;
         View     : Orka.glTF.Buffers.Buffer_View;
         Target   : not null Target_Array_Access)
      is
         use Orka.glTF.Accessors;

         Count : constant Positive := Attribute_Length (Accessor.Kind) * Accessor.Count;
         Bytes_Per_Element : constant Positive := Bytes_Element (Accessor.Component);
         pragma Assert (View.Length / Bytes_Per_Element = Count);
         pragma Assert (Source_Type'Size / System.Storage_Unit = Bytes_Per_Element);

         procedure Extract is new Orka.glTF.Buffers.Extract_From_Buffer (Source_Type, Source_Array);

         Source : Source_Array (Target.all'Range);
      begin
         Extract (View, Source);
         Target.all := Convert_Array (Source);
      end Get_Array;

   end Buffer_View_Conversions;

   procedure Count_Parts
     (Format : not null access Rendering.Vertex_Formats.Vertex_Format;
      Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : Orka.glTF.Meshes.Mesh_Vectors.Vector;
      Vertices, Indices : out Natural)
   is
      use type Ada.Containers.Count_Type;
      use type Orka.glTF.Accessors.Component_Kind;
      use type GL.Types.Unsigned_Numeric_Type;
      use Orka.glTF.Accessors;

      Count_Vertices : Natural := 0;
      Count_Indices  : Natural := 0;
   begin
      for Mesh of Meshes loop
         declare
            Primitives : Orka.glTF.Meshes.Primitive_Vectors.Vector renames Mesh.Primitives;
            pragma Assert (Primitives.Length = 1, "Mesh '" & Mesh.Name.all & "' has more than one primitive");

            First_Primitive : Orka.glTF.Meshes.Primitive renames Primitives (0);
            pragma Assert (First_Primitive.Attributes.Length = 3,
                           "Primitive of mesh " & Mesh.Name.all & " does not have 3 attributes");

            Attribute_Position : constant Natural := First_Primitive.Attributes ("POSITION");
            Attribute_Normal   : constant Natural := First_Primitive.Attributes ("NORMAL");
            Attribute_UV       : constant Natural := First_Primitive.Attributes ("TEXCOORD_0");

            pragma Assert (First_Primitive.Indices /= Orka.glTF.Undefined);
            Attribute_Index : constant Natural := First_Primitive.Indices;

            Accessor_Position : Accessor renames Accessors (Attribute_Position);
            Accessor_Normal   : Accessor renames Accessors (Attribute_Normal);
            Accessor_UV       : Accessor renames Accessors (Attribute_UV);

            pragma Assert (Accessor_Position.Component = Orka.glTF.Accessors.Float);
            pragma Assert (Accessor_Normal.Component = Orka.glTF.Accessors.Float);
            pragma Assert (Accessor_UV.Component = Orka.glTF.Accessors.Float);

            pragma Assert (Accessor_Position.Kind = Orka.glTF.Accessors.Vector3);
            pragma Assert (Accessor_Normal.Kind = Orka.glTF.Accessors.Vector3);
            pragma Assert (Accessor_UV.Kind = Orka.glTF.Accessors.Vector2);

            pragma Assert (Accessor_Position.Count = Accessor_Normal.Count);
            pragma Assert (Accessor_Position.Count = Accessor_UV.Count);

            Accessor_Index : Accessor renames Accessors (Attribute_Index);

            pragma Assert (Accessor_Index.Kind = Orka.glTF.Accessors.Scalar);

            pragma Assert (Unsigned_Type (Accessor_Index.Component) <= Format.Index_Kind,
                           "Index of mesh " & Mesh.Name.all & " has type " &
                             GL.Types.Unsigned_Numeric_Type'Image (Unsigned_Type (Accessor_Index.Component)) &
                             " but expected " &
                             GL.Types.Unsigned_Numeric_Type'Image (Format.Index_Kind) & " or lower");
         begin
            Count_Vertices := Count_Vertices + Accessor_Position.Count;
            Count_Indices  := Count_Indices + Accessor_Index.Count;
         end;
      end loop;

      Vertices := Count_Vertices;
      Indices := Count_Indices;
   end Count_Parts;

   procedure Add_Parts
     (Format : not null access Rendering.Vertex_Formats.Vertex_Format;
      Batch  : in out Rendering.Buffers.MDI.Batch;
      Views     : Orka.glTF.Buffers.Buffer_View_Vectors.Vector;
      Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : Orka.glTF.Meshes.Mesh_Vectors.Vector)
   is
      use GL.Types;
      use Orka.glTF.Accessors;
      use Orka.glTF.Buffers;

      pragma Assert (Format.Index_Kind = UInt_Type);
      package Index_Conversions is new Buffer_View_Conversions (UInt, UInt_Array, Indirect.UInt_Array_Access);
      --  TODO Use Format.Index_Kind

      package Vertex_Conversions is new Buffer_View_Conversions (Half, Half_Array, Indirect.Half_Array_Access);
      procedure Get_Singles is new Vertex_Conversions.Get_Array (Single, Single_Array, Orka.Types.Convert);

      function Cast (Value : UByte)  return UInt is (UInt (Value));
      function Cast (Value : UShort) return UInt is (UInt (Value));
      function Cast (Value : UInt)   return UInt is (Value);

      function Convert is new Index_Conversions.Convert_Array (UByte, UByte_Array, Cast);
      function Convert is new Index_Conversions.Convert_Array (UShort, UShort_Array, Cast);
      function Convert is new Index_Conversions.Convert_Array (UInt, UInt_Array, Cast);

      procedure Get_UBytes  is new Index_Conversions.Get_Array (UByte, UByte_Array, Convert);
      procedure Get_UShorts is new Index_Conversions.Get_Array (UShort, UShort_Array, Convert);
      procedure Get_UInts   is new Index_Conversions.Get_Array (UInt, UInt_Array, Convert);
   begin
      for Mesh of Meshes loop
         declare
            First_Primitive : Orka.glTF.Meshes.Primitive renames Mesh.Primitives (0);

            Attribute_Position : constant Natural := First_Primitive.Attributes ("POSITION");
            Attribute_Normal   : constant Natural := First_Primitive.Attributes ("NORMAL");
            Attribute_UV       : constant Natural := First_Primitive.Attributes ("TEXCOORD_0");

            Attribute_Index    : constant Natural := First_Primitive.Indices;

            Accessor_Position : Accessor renames Accessors (Attribute_Position);
            Accessor_Normal   : Accessor renames Accessors (Attribute_Normal);
            Accessor_UV       : Accessor renames Accessors (Attribute_UV);

            Accessor_Index    : Accessor renames Accessors (Attribute_Index);

            View_Position : Buffer_View renames Views (Accessor_Position.View);
            View_Normal   : Buffer_View renames Views (Accessor_Normal.View);
            View_UV       : Buffer_View renames Views (Accessor_UV.View);

            View_Index : Buffer_View renames Views (Accessor_Index.View);

            Positions : Indirect.Half_Array_Access := new Half_Array (1 .. Int
                                                                      (Accessor_Position.Count * Attribute_Length (Accessor_Position.Kind)));
            Normals   : Indirect.Half_Array_Access := new Half_Array (1 .. Int
                                                                      (Accessor_Normal.Count * Attribute_Length (Accessor_Normal.Kind)));
            UVs       : Indirect.Half_Array_Access := new Half_Array (1 .. Int
                                                                      (Accessor_UV.Count * Attribute_Length (Accessor_UV.Kind)));

            Indices   : Indirect.UInt_Array_Access := new UInt_Array (1 .. Int
                                                                      (Accessor_Index.Count));
            --  TODO Use Conversions.Target_Array?
         begin
            --  Convert attributes
            Get_Singles (Accessor_Position, View_Position, Positions);
            Get_Singles (Accessor_Normal, View_Normal, Normals);
            Get_Singles (Accessor_UV, View_UV, UVs);

            --  Convert indices
            case Unsigned_Type (Accessor_Index.Component) is
               when GL.Types.UByte_Type =>
                  Get_UBytes (Accessor_Index, View_Index, Indices);
               when GL.Types.UShort_Type =>
                  Get_UShorts (Accessor_Index, View_Index, Indices);
               when GL.Types.UInt_Type =>
                  Get_UInts (Accessor_Index, View_Index, Indices);
            end case;

            Batch.Append (Positions, Normals, UVs, Indices);

            --  Deallocate buffers after use
            Indirect.Free_Array (Positions);
            Indirect.Free_Array (Normals);
            Indirect.Free_Array (UVs);
            Indirect.Free_Array (Indices);
         end;
      end loop;
   end Add_Parts;

--     function Load_Model
--       (Format     : not null access Orka.Rendering.Vertex_Formats.Vertex_Format;
--        Uniform_WT : not null access Orka.Rendering.Programs.Uniforms.Uniform_Sampler;
--        Uniform_IO : not null access Orka.Rendering.Programs.Uniforms.Uniform;
--        Path       : String) return Model
--     is
--     begin
--        begin
--           declare
--              use GL.Objects.Buffers;
--
--              Vertices_Length, Indices_Length : Natural;
--              T1, T2, T3, T4, T5, T6 : Ada.Real_Time.Time := Ada.Real_Time.Clock;
--              Meshes_Length : Ada.Containers.Count_Type := 0;
--           begin
--              return Object : Model
--                := (Scene      => new Model_Scene'(Scene => Trees.Create_Tree (Default_Root_Name), Shapes => <>),
--                    Format     => Format.all'Unrestricted_Access,
--                    Uniform_WT => Uniform_WT.all'Unrestricted_Access,
--                    Uniform_IO => Uniform_IO.all'Unrestricted_Access,
--                    others     => <>)
--              do
--                 T5 := Ada.Real_Time.Clock;
--
--                 Count_Parts (Format, Accessors, Meshes, Vertices_Length, Indices_Length);
--
--                 Object.Bounds := Orka.Rendering.Buffers.Create_Buffer
--                   (Flags => Storage_Bits'(others => False),
--                    Data  => Bounds_List (Accessors, Meshes));
--
--                 Object.TBO_BB.Attach_Buffer (GL.Pixels.RGBA32F, Object.Bounds.GL_Buffer);
--
--                 Object.Batch := Orka.Rendering.Buffers.MDI.Create_Batch
--                   (Positive (Parts.Length), Vertices_Length, Indices_Length,
--                    Format  => Format,
--                    Flags   => Storage_Bits'(Dynamic_Storage => True, others => False),
--                    Visible => True);
--
--                 Add_Parts (Format, Object.Batch, Buffer_Views, Accessors, Meshes);
--                 T6 := Ada.Real_Time.Clock;
--
--                 declare
--                    use type Ada.Real_Time.Time;
--                    use Ada.Real_Time;
--
--                    Reading_Time    : constant Duration := 1e3 * To_Duration (T2 - T1);
--                    Parsing_Time    : constant Duration := 1e3 * To_Duration (T3 - T2);
--                    Processing_Time : constant Duration := 1e3 * To_Duration (T4 - T3);
--                    Scene_Tree_Time : constant Duration := 1e3 * To_Duration (T5 - T4);
--                    Buffers_Time    : constant Duration := 1e3 * To_Duration (T6 - T5);
--                    Loading_Time    : constant Duration := 1e3 * To_Duration (T6 - T1);
--                 begin
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "Loaded model " & Path);
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           " " & Ada.Containers.Count_Type'Image (Meshes_Length) & " parts," &
--                                             Natural'Image (Vertices_Length) & " vertices," &
--                                             Natural'Image (Indices_Length) & " indices");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "  loaded in" & Duration'Image (Loading_Time) & " ms");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "    reading file:" & Duration'Image (Reading_Time) & " ms");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "    parsing JSON:" & Duration'Image (Parsing_Time) & " ms");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "    processing glTF:" & Duration'Image (Processing_Time) & " ms");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "    scene tree:" & Duration'Image (Scene_Tree_Time) & " ms");
--                    Debug_Messages.Insert (GL.Debug.Notification,
--                                           "    buffers:" & Duration'Image (Buffers_Time) & " ms");
--                 end;
--
--              end return;
--           end;
--        end;
--     end Load_Model;

   procedure Load
     (Bytes   : in out Byte_Array_Access;
      Time    : Ada.Real_Time.Time_Span;
      Path    : SU.Unbounded_String;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      Job : constant Jobs.Job_Ptr := new GLTF_Parse_Job'
        (Jobs.Abstract_Job with Bytes => Bytes, Path => Path, Reading_Time => Time);
   begin
      Enqueue (Job);
   end Load;

   overriding
   procedure Execute
     (Object  : GLTF_Parse_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      use type Ada.Real_Time.Time;

      package Parsers is new JSON.Parsers (Orka.glTF.Types);

      Bytes : Byte_Array_Access := Object.Bytes;
      Path  : String renames SU.To_String (Object.Path);
   begin
      declare
         T1 : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

         --  Tokenize and parse JSON data
         Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Bytes);
         Data : GLTF_Data_Access := new GLTF_Data'
           (JSON   => new JSON_Value'Class'(Parsers.Parse (Stream)),
            others => <>);

         T2 : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

         Asset : constant JSON_Object_Value := Data.JSON.Get_Object ("asset");
      begin
         --  Require glTF 2.x
         if Asset.Get ("version").Value /= "2.0" then
            raise Model_Load_Error with "glTF file '" & Path & "' does not use glTF 2.0";
         end if;
         --  TODO Check minVersion

         declare
            Finish_Job : constant Jobs.Job_Ptr := new GLTF_Finish_Processing_Job'
              (Jobs.Abstract_Job with Data => Data, Path => Object.Path, Start_Time => T2);

            Process_1_Job : constant Jobs.Job_Ptr := new GLTF_Process_Buffers_Job'
              (Jobs.Abstract_Job with Data => Data);
            Process_2_Job : constant Jobs.Job_Ptr := new GLTF_Process_Accessors_Job'
              (Jobs.Abstract_Job with Data => Data);
            Process_3_Job : constant Jobs.Job_Ptr := new GLTF_Process_Meshes_Job'
              (Jobs.Abstract_Job with Data => Data);
            Process_4_Job : constant Jobs.Job_Ptr := new GLTF_Process_Nodes_Job'
              (Jobs.Abstract_Job with Data => Data);
         begin
            Finish_Job.Set_Dependencies
              ((Process_1_Job, Process_2_Job, Process_3_Job, Process_4_Job));

            Data.Reading_Time := Object.Reading_Time;
            Data.Parsing_Time := T2 - T1;

            Enqueue (Process_1_Job);
            Enqueue (Process_2_Job);
            Enqueue (Process_3_Job);
            Enqueue (Process_4_Job);
         end;
      exception
         when others =>
            declare
               JSON2 : JSON_Value_Access := Data.JSON;
               Data2 : GLTF_Data_Access  := Data;
            begin
               Free_JSON (JSON2);
               Free_Data (Data2);
            end;
            raise;
      end;
      Free_Byte_Array (Bytes);
   exception
      when others =>
         Free_Byte_Array (Bytes);
         raise;
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Process_Buffers_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      Buffers : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("buffers"));
      Views   : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("bufferViews"));
   begin
      Object.Data.Buffers := Orka.glTF.Buffers.Get_Buffers (Buffers);
      Object.Data.Views   := Orka.glTF.Buffers.Get_Buffer_Views (Object.Data.Buffers, Views);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Process_Accessors_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      Accessors : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("accessors"));
   begin
      Object.Data.Accessors := Orka.glTF.Accessors.Get_Accessors (Accessors);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Process_Meshes_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      Meshes : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("meshes"));
   begin
      Object.Data.Meshes := Orka.glTF.Meshes.Get_Meshes (Meshes);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Process_Nodes_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      Nodes  : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("nodes"));
      Scenes : JSON_Array_Value renames JSON_Array_Value (Object.Data.JSON.Get ("scenes"));
   begin
      Object.Data.Nodes  := Orka.glTF.Scenes.Get_Nodes (Nodes);
      Object.Data.Scenes := Orka.glTF.Scenes.Get_Scenes (Scenes);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Finish_Processing_Job;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr))
   is
      use Orka.glTF.Types;
      use type Ada.Real_Time.Time;

      --  TODO Textures, Images, Samplers, Materials, Cameras

      Default_Scene_Index : constant Long_Integer := Object.Data.JSON.Get ("scene").Value;
      Default_Scene : constant Orka.glTF.Scenes.Scene
        := Object.Data.Scenes (Natural (Default_Scene_Index));
      --  Cannot be "renames" because freeing Object.Data results in cursor tampering

      Path : String renames SU.To_String (Object.Path);
   begin
      --  TODO Deallocate any strings allocated in Orka.glTF.*
      if Default_Scene.Nodes.Is_Empty then
         raise Model_Load_Error with "glTF file '" & Path & "' has an empty scene";
      end if;

      Object.Data.Process_Time := Ada.Real_Time.Clock - Object.Start_Time;

      declare
         Scene_Data : constant Model_Scene_Ptr := new Model_Scene'
           (Scene => Trees.Create_Tree (Default_Root_Name), others => <>);
         Scene : Trees.Tree renames Scene_Data.Scene;

         Parts : String_Maps.Map;

         use type GL.Types.Single;
         use Transforms;
         Start_Time : Ada.Real_Time.Time;

         --  Convert the object from structural frame (X = aft,
         --  Y = right, Z = top) to OpenGL (X = right, Y = top,
         --  Z = aft)
         --
         --  X => Z, Y => X, Z => Y   -->   Ry (-90.0) * Rx (-90.0)
         Structural_Frame_To_GL : constant Trees.Matrix4 := Ry (-90.0);
         --  The Khronos Blender glTF 2.0 exporter seems to already apply one of the rotations
      begin
         Start_Time := Ada.Real_Time.Clock;

         Scene.Set_Local_Transform
           (Scene.To_Cursor (Scene.Root_Name), Structural_Frame_To_GL);

         --  Link the nodes in the default scene to the root node and
         --  then add all the other nodes that are reachable
         for Node_Index of Default_Scene.Nodes loop
            Scene.Add_Node (Object.Data.Nodes (Node_Index).Name.all, Scene.Root_Name);
         end loop;
         Add_Nodes (Scene, Parts, Object.Data.Nodes, Default_Scene.Nodes);

         Scene_Data.Shapes := Shape_List (Parts, Scene);

         Object.Data.Scene_Time := Ada.Real_Time.Clock - Start_Time;
         --  TODO Do something with Scene_Data and copy Object.Data.*_Time

         declare
            JSON : JSON_Value_Access := Object.Data.JSON;
            Data : GLTF_Data_Access := Object.Data;
         begin
            Free_JSON (JSON);
            Free_Data (Data);
         end;
      end;
   exception
      when Error : others =>
         declare
            JSON : JSON_Value_Access := Object.Data.JSON;
            Data : GLTF_Data_Access  := Object.Data;
         begin
            Free_JSON (JSON);
            Free_Data (Data);
         end;
         raise;
   end Execute;

end Orka.Resources.Models.glTF;
