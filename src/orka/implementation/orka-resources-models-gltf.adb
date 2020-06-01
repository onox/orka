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

with System;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

with JSON.Parsers;
with JSON.Streams;

with GL.Types.Indirect;

with Orka.glTF.Buffers;
with Orka.glTF.Accessors;
with Orka.glTF.Meshes;
with Orka.glTF.Scenes;

with Orka.Jobs;
with Orka.Logging;
with Orka.Resources.Locations;
with Orka.Types;

package body Orka.Resources.Models.glTF is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;

   package Messages is new Orka.Logging.Messages (Resource_Loader);

   Default_Root_Name : constant String := "root";

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Natural,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   -----------------------------------------------------------------------------

   use Ada.Real_Time;

   type Times_Data is record
      Reading    : Time_Span;
      Parsing    : Time_Span;
      Processing : Time_Span;
      Scene      : Time_Span;
      Buffers    : Time_Span;
   end record;

   type GLTF_Data (Maximum_Nodes, Maximum_Accessors : Natural) is limited record
      Directory : SU.Unbounded_String;
      Location  : Locations.Location_Ptr;
      Buffers   : Orka.glTF.Buffers.Buffer_Vectors.Vector (Capacity => 8);
      Views     : Orka.glTF.Buffers.Buffer_View_Vectors.Vector (Capacity => Maximum_Accessors);
      Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector  (Capacity => Maximum_Accessors);
      Meshes    : Orka.glTF.Meshes.Mesh_Vectors.Vector (Capacity => Maximum_Nodes);
      Nodes     : Orka.glTF.Scenes.Node_Vectors.Vector (Capacity => Maximum_Nodes);
      Scenes    : Orka.glTF.Scenes.Scene_Vectors.Vector (Capacity => 8);
      Default_Scene : Long_Integer;
      Times     : Times_Data := (others => Time_Span_Zero);
      Start_Time : Time;
      Manager   : Managers.Manager_Ptr;
   end record;

   type GLTF_Data_Access is access GLTF_Data;

   procedure Free_Data is new Ada.Unchecked_Deallocation
     (Object => GLTF_Data, Name => GLTF_Data_Access);

   package GLTF_Data_Pointers is new Orka.Smart_Pointers
     (GLTF_Data, GLTF_Data_Access, Free_Data);

   -----------------------------------------------------------------------------

   type GLTF_Parse_Job is new Jobs.Abstract_Job with record
      Data     : Loaders.Resource_Data;
      Manager  : Managers.Manager_Ptr;
      Location : Locations.Location_Ptr;
   end record;

   type GLTF_Finish_Processing_Job is new Jobs.Abstract_Job with record
      Data : GLTF_Data_Pointers.Mutable_Pointer;
      Path : SU.Unbounded_String;
   end record;

   type GLTF_Create_Model_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Data  : GLTF_Data_Pointers.Mutable_Pointer;
      Path  : SU.Unbounded_String;
      Scene : Model_Scene_Ptr;
      Vertices, Indices : Natural;
   end record;

   type GLTF_Write_Buffers_Job is new Jobs.Abstract_Job with record
      Data  : GLTF_Data_Pointers.Mutable_Pointer;
      Model : Model_Ptr;
   end record;

   type GLTF_Finish_Loading_Job is new Jobs.Abstract_Job and Jobs.GPU_Job with record
      Data  : GLTF_Data_Pointers.Mutable_Pointer;
      Path  : SU.Unbounded_String;
      Model : Model_Ptr;
      Parts, Vertices, Indices : Natural;
      Start_Time : Ada.Real_Time.Time;
   end record;

   -----------------------------------------------------------------------------
   --                            EXECUTE PROCEDURES                           --
   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object  : GLTF_Parse_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : GLTF_Finish_Processing_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : GLTF_Create_Model_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : GLTF_Write_Buffers_Job;
      Context : Jobs.Execution_Context'Class);

   overriding
   procedure Execute
     (Object  : GLTF_Finish_Loading_Job;
      Context : Jobs.Execution_Context'Class);

   -----------------------------------------------------------------------------

   procedure Add_Nodes
     (Scene   : in out Trees.Tree;
      Parts   : in out String_Maps.Map;
      Nodes   : in out Orka.glTF.Scenes.Node_Vectors.Vector;
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
               Parent_Name : String renames SU.To_String (Parent_Node.Name);

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
                  Scene.Add_Node (Nodes (Child_Index).Name, Parent_Name);
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

   function Mesh_Node_Cursors
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
   end Mesh_Node_Cursors;

   function Bounds_List
     (Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : in out Orka.glTF.Meshes.Mesh_Vectors.Vector) return Orka.Types.Singles.Vector4_Array
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

         procedure Extract is new Orka.glTF.Buffers.Extract_From_Buffer
           (Source_Type, Source_Array);

         type Source_Array_Access is access Source_Array;

         procedure Free_Array is new Ada.Unchecked_Deallocation
           (Object => Source_Array, Name => Source_Array_Access);

         Source : Source_Array_Access := new Source_Array (Target.all'Range);
      begin
         Extract (View, Source.all);
         Target.all := Convert_Array (Source.all);
         Free_Array (Source);
      end Get_Array;

   end Buffer_View_Conversions;

   procedure Count_Parts
     (Index_Kind : GL.Types.Index_Type;
      Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : in out Orka.glTF.Meshes.Mesh_Vectors.Vector;
      Vertices, Indices : out Natural)
   is
      use type Ada.Containers.Count_Type;
      use type Orka.glTF.Accessors.Component_Kind;
      use type GL.Types.Index_Type;
      use Orka.glTF.Accessors;

      Count_Vertices : Natural := 0;
      Count_Indices  : Natural := 0;
   begin
      for Mesh of Meshes loop
         declare
            Mesh_Name : String renames SU.To_String (Mesh.Name);

            Primitives : Orka.glTF.Meshes.Primitive_Vectors.Vector renames Mesh.Primitives;
            pragma Assert (Primitives.Length = 1, "Mesh '" & Mesh_Name & "' has more than one primitive");

            First_Primitive : Orka.glTF.Meshes.Primitive renames Primitives (0);
            pragma Assert (First_Primitive.Attributes.Length = 3,
              "Primitive of mesh " & Mesh_Name & " does not have 3 attributes");

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

            pragma Assert (Unsigned_Type (Accessor_Index.Component) <= Index_Kind,
              "Index of mesh " & Mesh_Name & " has type " &
              GL.Types.Index_Type'Image (Unsigned_Type (Accessor_Index.Component)) &
              " but expected " & Index_Kind'Image & " or lower");
         begin
            Count_Vertices := Count_Vertices + Accessor_Position.Count;
            Count_Indices  := Count_Indices + Accessor_Index.Count;
         end;
      end loop;

      Vertices := Count_Vertices;
      Indices  := Count_Indices;
   end Count_Parts;

   procedure Add_Parts
     (Batch  : in out Rendering.Buffers.MDI.Batch;
      Views     : Orka.glTF.Buffers.Buffer_View_Vectors.Vector;
      Accessors : Orka.glTF.Accessors.Accessor_Vectors.Vector;
      Meshes    : in out Orka.glTF.Meshes.Mesh_Vectors.Vector)
   is
      use GL.Types;
      use Orka.glTF.Accessors;
      use Orka.glTF.Buffers;
      use all type Orka.Types.Element_Type;

      package Index_Conversions is new Buffer_View_Conversions (UInt, UInt_Array, Indirect.UInt_Array_Access);

      package Vertex_Conversions is new Buffer_View_Conversions (Half, Half_Array, Indirect.Half_Array_Access);
      procedure Get_Singles is new Vertex_Conversions.Get_Array (Single, Single_Array, Orka.Types.Convert);

      function Cast (Value : UShort) return UInt is (UInt (Value));
      function Cast (Value : UInt)   return UInt is (Value);

      function Convert is new Index_Conversions.Convert_Array (UShort, UShort_Array, Cast);
      function Convert is new Index_Conversions.Convert_Array (UInt, UInt_Array, Cast);

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

   -----------------------------------------------------------------------------
   --                                 Loader                                  --
   -----------------------------------------------------------------------------

   type GLTF_Loader is limited new Loaders.Loader with record
      Manager : Managers.Manager_Ptr;
   end record;

   overriding
   function Extension (Object : GLTF_Loader) return Loaders.Extension_String is ("gltf");

   overriding
   procedure Load
     (Object   : GLTF_Loader;
      Data     : Loaders.Resource_Data;
      Enqueue  : not null access procedure (Element : Jobs.Job_Ptr);
      Location : Locations.Location_Ptr)
   is
      Job : constant Jobs.Job_Ptr := new GLTF_Parse_Job'
        (Jobs.Abstract_Job with
          Data     => Data,
          Manager  => Object.Manager,
          Location => Location);
   begin
      Enqueue (Job);
   end Load;

   function Create_Loader
     (Manager : Managers.Manager_Ptr) return Loaders.Loader_Ptr
   is (new GLTF_Loader'(Manager => Manager));

   -----------------------------------------------------------------------------

   overriding
   procedure Execute
     (Object  : GLTF_Parse_Job;
      Context : Jobs.Execution_Context'Class)
   is
      use Orka.glTF.Types;

      package Parsers is new JSON.Parsers (Orka.glTF.Types);

      Path  : String renames SU.To_String (Object.Data.Path);

      Stream : constant JSON.Streams.Stream'class
        := JSON.Streams.Create_Stream (Object.Data.Bytes.Get.Value);
      Parser : Parsers.Parser := Parsers.Create (Stream);
   begin
      declare
         T1 : constant Time := Clock;

         JSON : constant Orka.glTF.Types.JSON_Value := Parser.Parse;

         T2 : constant Time := Clock;

         Buffers   : constant JSON_Value := JSON ("buffers");
         Views     : constant JSON_Value := JSON ("bufferViews");
         Accessors : constant JSON_Value := JSON ("accessors");
         Meshes    : constant JSON_Value := JSON ("meshes");
         Nodes     : constant JSON_Value := JSON ("nodes");
         Scenes    : constant JSON_Value := JSON ("scenes");

         Maximum_Nodes : constant Natural := Nodes.Length;

         --  Tokenize and parse JSON data
         Data : constant GLTF_Data_Access := new GLTF_Data'
           (Maximum_Nodes     => Maximum_Nodes,
            Maximum_Accessors => Maximum_Nodes * 4,
            Directory  => SU.To_Unbounded_String (Ada.Directories.Containing_Directory (Path)),
            Location   => Object.Location,
            Manager    => Object.Manager,
            Start_Time => Object.Data.Start_Time,
            others     => <>);

         Asset : constant JSON_Value := JSON ("asset");
         Scene : constant JSON_Value := JSON ("scene");

         function Load_Data (Path : String) return Byte_Array_Pointers.Pointer is
            Directory     : String renames SU.To_String (Data.Directory);
            Relative_Path : constant String := Directory & Locations.Path_Separator & Path;
         begin
            return Data.Location.Read_Data (Relative_Path);
         end Load_Data;

         Pointer : GLTF_Data_Pointers.Mutable_Pointer;
      begin
         --  Require glTF 2.x
         if Asset.Get ("version").Value /= "2.0" then
            raise Model_Load_Error with "glTF file '" & Path & "' does not use glTF 2.0";
         end if;
         --  TODO Check minVersion

         --  Process buffers, nodes, meshes, and scenes
         Data.Buffers.Append (Orka.glTF.Buffers.Get_Buffers (Buffers, Load_Data'Access));
         Data.Views.Append (Orka.glTF.Buffers.Get_Buffer_Views (Data.Buffers, Views));

         Data.Accessors.Append (Orka.glTF.Accessors.Get_Accessors (Accessors));
         Data.Meshes.Append (Orka.glTF.Meshes.Get_Meshes (Meshes));

         Data.Nodes.Append (Orka.glTF.Scenes.Get_Nodes (Nodes));
         Data.Scenes.Append (Orka.glTF.Scenes.Get_Scenes (Scenes));

         Data.Default_Scene := Scene.Value;

         Data.Times.Reading    := Object.Data.Reading_Time;
         Data.Times.Parsing    := T2 - T1;
         Data.Times.Processing := Clock - T2;

         Pointer.Set (Data);

         declare
            Finish_Job : constant Jobs.Job_Ptr := new GLTF_Finish_Processing_Job'
              (Jobs.Abstract_Job with Data => Pointer, Path => Object.Data.Path);
         begin
            Context.Enqueue (Finish_Job);
         end;
      end;
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Finish_Processing_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Data  : GLTF_Data renames Object.Data.Get;

      --  TODO Textures, Images, Samplers, Materials, Cameras

      Default_Scene_Index : constant Long_Integer := Data.Default_Scene;
      Default_Scene : constant Orka.glTF.Scenes.Scene
        := Data.Scenes (Natural (Default_Scene_Index));
      --  Cannot be "renames" because freeing Object.Data results in cursor tampering

      Path : String renames SU.To_String (Object.Path);
   begin
      if Default_Scene.Nodes.Is_Empty then
         raise Model_Load_Error with "glTF file '" & Path & "' has an empty scene";
      end if;

      declare
         Scene_Data : constant Model_Scene_Ptr := new Model_Scene'
           (Scene => Trees.Create_Tree (Default_Root_Name), others => <>);
         Scene : Trees.Tree renames Scene_Data.Scene;

         Parts : String_Maps.Map;

         Start_Time : constant Time := Clock;

         Vertices, Indices : Natural;
      begin
         --  Link the nodes in the default scene to the root node and
         --  then add all the other nodes that are reachable
         for Node_Index of Default_Scene.Nodes loop
            Scene.Add_Node (Data.Nodes (Node_Index).Name, Scene.Root_Name);
         end loop;
         Add_Nodes (Scene, Parts, Data.Nodes, Default_Scene.Nodes);

         --  Collect an array of cursors to nodes in the scene for nodes
         --  that have a corresponding mesh part. This is needed so that,
         --  after updating the whole scene tree, the world transforms of
         --  these nodes can be copied to a GPU buffer before rendering.
         Scene_Data.Shapes := Mesh_Node_Cursors (Parts, Scene);

         if Scene_Data.Shapes.Element'Length = 0 then
            --  TODO Free Scene_Data
            raise Model_Load_Error with "glTF file '" & Path & "' has no mesh parts";
         end if;

         Data.Times.Scene := Clock - Start_Time;

         --  Count total number of vertices and indices
         Count_Parts (GL.Types.UInt_Type, Data.Accessors,
           Data.Meshes, Vertices, Indices);

         declare
            Create_Job : constant Jobs.Job_Ptr := new GLTF_Create_Model_Job'
              (Jobs.Abstract_Job with Data => Object.Data, Path => Object.Path,
                Scene => Scene_Data, Vertices => Vertices, Indices => Indices);
         begin
            Context.Enqueue (Create_Job);
         end;
      end;
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Create_Model_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Data  : GLTF_Data renames Object.Data.Get;
      Parts : constant Positive := Object.Scene.Shapes.Element'Length;

      Start_Time : constant Time := Clock;

      Model_Data : constant Model_Ptr := new Model'
        (Scene  => Object.Scene,
         Batch  => Rendering.Buffers.MDI.Create_Batch
           (Parts, Object.Vertices, Object.Indices),
         --  Bounding boxes for culling
         Bounds => Rendering.Buffers.Create_Buffer
           (Flags => (others => False),
            Data  => Bounds_List (Data.Accessors, Data.Meshes)));

      Buffers_Job : constant Jobs.Job_Ptr := new GLTF_Write_Buffers_Job'
        (Jobs.Abstract_Job with Data => Object.Data, Model => Model_Data);

      Finish_Job : constant Jobs.Job_Ptr := new GLTF_Finish_Loading_Job'
        (Jobs.Abstract_Job with Data => Object.Data, Path => Object.Path,
         Model => Model_Data, Start_Time => Start_Time,
         Parts => Parts, Vertices => Object.Vertices, Indices => Object.Indices);
   begin
      Orka.Jobs.Chain ((Buffers_Job, Finish_Job));
      Context.Enqueue (Buffers_Job);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Write_Buffers_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Data : GLTF_Data renames Object.Data.Get;
   begin
      Add_Parts (Object.Model.Batch, Data.Views, Data.Accessors, Data.Meshes);
   end Execute;

   overriding
   procedure Execute
     (Object  : GLTF_Finish_Loading_Job;
      Context : Jobs.Execution_Context'Class)
   is
      Data : GLTF_Data renames Object.Data.Get;
      Path : String renames SU.To_String (Object.Path);
   begin
      Object.Model.Batch.Finish_Batch;

      Data.Times.Buffers := Clock - Object.Start_Time;

      --  Register the model at the resource manager
      Data.Manager.Add_Resource (Path, Resource_Ptr (Object.Model));

      declare
         Times : Times_Data renames Data.Times;
      begin
         Messages.Log (Info, "Loaded model " & Path & " in " &
           Logging.Trim (Logging.Image (Clock - Data.Start_Time)));
         Messages.Log (Info, " " &
           Object.Parts'Image & " parts," &
           Object.Vertices'Image & " vertices," &
           Object.Indices'Image & " indices");
         Messages.Log (Info, "  statistics:");
         Messages.Log (Info, "    reading file:    " & Logging.Image (Times.Reading));
         Messages.Log (Info, "    parsing JSON:    " & Logging.Image (Times.Parsing));
         Messages.Log (Info, "    processing glTF: " & Logging.Image (Times.Processing));
         Messages.Log (Info, "    scene tree:      " & Logging.Image (Times.Scene));
         Messages.Log (Info, "    buffers:         " & Logging.Image (Times.Buffers));
      end;
   end Execute;

end Orka.Resources.Models.glTF;
