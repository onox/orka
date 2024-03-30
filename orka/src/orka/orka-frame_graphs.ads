--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 - 2022 onox <denkpadje@gmail.com>
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

with Ada.Strings.Bounded;

with Orka.Contexts;
with Orka.Windows;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs;
with Orka.Rendering.States;
with Orka.Rendering.Textures;
with Orka.Resources.Locations;

private with Ada.Containers.Indefinite_Holders;

private with GL.Buffers;

private with Orka.Containers.Bounded_Vectors;

package Orka.Frame_Graphs is
   pragma Preelaborate;

   Maximum_Name_Length : constant := 32;

   Maximum_External_Resources : constant := 8;
   --  Maximum number of resources which can be imported or exported

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Maximum_Name_Length);

   function "+" (Value : String) return Name_Strings.Bounded_String;
   function "+" (Value : Name_Strings.Bounded_String) return String;

   use all type Rendering.Textures.LE.Texture_Kind;

   ----------------------------------------------------------------------

   type Resource_ID is private;

   type Resource_Version is private;

   type Resource is record
      Name        : Name_Strings.Bounded_String;
      Description : Rendering.Textures.Texture_Description;
      ID          : Resource_ID;
      Version     : Resource_Version;
   end record;

   type Resource_View (Object : not null access constant Resource) is record
      Layer : Natural;
   end record
     with Dynamic_Predicate =>
       Rendering.Textures.Get_Format_Kind (Object.Description.Format) = Color
         and (Rendering.Textures.Has_Layers (Object.Description.Kind)
                and then Layer < Rendering.Textures.Layers (Object.Description.Kind, Object.Description.Size));

--   function Level (Object : aliased Resource; Level : Natural) return Resource
--     with Pre  => Level in 0 .. Object.Description.Levels - 1,
--          Post => Frame_Graphs.Level'Result.Description.Levels = 1;

--   function Resolve (Object : Resource) return Resource
--     with Pre  => Object.Kind in Texture_2D_Multisample | Texture_2D_Multisample_Array,
--          Post => (case Object.Description.Kind is
--                     when Texture_2D_Multisample       => Resolve'Result.Description.Kind = Texture_2D,
--                     when Texture_2D_Multisample_Array => Resolve'Result.Description.Kind = Texture_2D_Array,
--                     when others => raise Program_Error);

   ----------------------------------------------------------------------

   type Read_Mode is (Not_Used, Framebuffer_Attachment, Texture_Read, Image_Load);

   type Write_Mode is (Not_Used, Framebuffer_Attachment, Image_Store);

   type Read_Write_Mode is (Not_Used, Framebuffer_Attachment, Image_Load_Store);

   type Binding_Point is new Natural;

   subtype Attachment_Point is Binding_Point range 0 .. 7;

   ----------------------------------------------------------------------

   use all type Rendering.Textures.Format_Kind;

   type Render_Pass (<>) is tagged limited private;

   function Name (Object : Render_Pass) return String;

   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Mode    : Read_Mode;
      Binding : Binding_Point)
   with Pre => (if Mode = Framebuffer_Attachment then
                  Binding in Attachment_Point
                    and Rendering.Textures.Get_Format_Kind (Subject.Description.Format) /= Color);
   --  Add the given resource as an input to the render pass, so that it
   --  can be read as a texture, an image, or used as an attachment of a framebuffer

   procedure Add_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Mode    : Write_Mode;
      Binding : Binding_Point)
   with Pre => (if Mode = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an output to the render pass, so that it
   --  can be written as an image or used as an attachment of a framebuffer

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Mode    : Read_Write_Mode;
      Binding : Binding_Point) return Resource
   with Pre => (if Mode = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an input and an output to the render pass,
   --  indicating that the resource will be modified by the pass. The modified
   --  resource is returned and may be used as an input for other render passes.

   ----------------------------------------------------------------------

   procedure Add_Input
     (Object  : Render_Pass;
      View    : Resource_View;
      Mode    : Read_Mode;
      Binding : Binding_Point)
   with Pre => Mode /= Framebuffer_Attachment;
   --  Add the given view of a resource as an input to the render pass, so that it
   --  can be read as a texture or an image

   procedure Add_Output
     (Object  : Render_Pass;
      View    : Resource_View;
      Mode    : Write_Mode;
      Binding : Binding_Point)
   with Pre => (if Mode = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an output to the render pass, so that it
   --  can be written as an image or used as an attachment of a framebuffer

   ----------------------------------------------------------------------

   type Program_Callback is limited interface;

   procedure Run (Object : Program_Callback; Program : Rendering.Programs.Program) is abstract;

   type Program_Callback_Access is access constant Program_Callback'Class;

   type Render_Pass_Index is new Positive;
   type Handle_Type is new Positive;

   type Frame_Graph
     (Maximum_Passes    : Render_Pass_Index;
      Maximum_Handles   : Positive;
      Maximum_Resources : Handle_Type) is tagged private;

   function Add_Pass
     (Object   : in out Frame_Graph;
      Name     : String;
      State    : Rendering.States.State;
      Program  : Rendering.Programs.Program;
      Callback : not null Program_Callback_Access;
      Side_Effect : Boolean := False) return Render_Pass'Class
   with Pre => Name'Length <= Maximum_Name_Length;

   function Contains (Object : Frame_Graph; Subject : Resource) return Boolean;

   function Importable (Object : Frame_Graph; Subject : Resource) return Boolean;
   function Exportable (Object : Frame_Graph; Subject : Resource) return Boolean;

   type Resource_Array is array (Positive range <>) of aliased Resource;

   procedure Import (Object : in out Frame_Graph; Subjects : Resource_Array)
     with Pre => Subjects'Length <= Maximum_External_Resources;
   procedure Export (Object : in out Frame_Graph; Subjects : Resource_Array)
     with Pre => Subjects'Length <= Maximum_External_Resources;
   --  Import and export the given resources
   --
   --  Importing and exporting resources is useful if the frame graph is used
   --  as a sub-graph of a larger graph.

   type External_Resources (Imported_Count, Exported_Count : Natural) is record
      Imported : Resource_Array (1 .. Imported_Count);
      Exported : Resource_Array (1 .. Exported_Count);
   end record;

   function Connect
     (Object   : in out Frame_Graph;
      Subject  : Frame_Graph;
      From, To : Resource_Array) return External_Resources
   with Pre => From'Length > 0 and then (for all Resource of From => Object.Exportable (Resource)) and then
               To'Length > 0 and then (for all Resource of To   => Object.Importable (Resource));

   function Connect
     (Object  : in out Frame_Graph;
      Subject : Frame_Graph;
      From    : Resource_Array) return Resource_Array
   with Pre => From'Length > 0 and then (for all Resource of From => Object.Exportable (Resource));

   procedure Write_Graph
     (Object   : Frame_Graph;
      Location : Resources.Locations.Writable_Location_Ptr;
      Path     : String);

   ----------------------------------------------------------------------

   type Renderable_Graph
     (Maximum_Passes    : Render_Pass_Index;
      Maximum_Resources : Handle_Type;
      Graph             : not null access constant Frame_Graphs.Frame_Graph) is tagged limited private
   with Type_Invariant => Renderable_Graph.Maximum_Passes = Renderable_Graph.Graph.Maximum_Passes
                            and Renderable_Graph.Maximum_Resources = Renderable_Graph.Graph.Maximum_Resources;

   procedure Render
     (Object  : in out Renderable_Graph;
      Window  : Orka.Windows.Window'Class;
      Present : Resource;
      Location : Resources.Locations.Location_Ptr);
   --  Render the resource which must be presented to the given window

   function Render
     (Object   : in out Renderable_Graph;
      Context  : in out Orka.Contexts.Context'Class;
      Present  : Resource;
      Location : Resources.Locations.Location_Ptr) return Orka.Rendering.Textures.Texture;
   --  Render the resource which must be presented and return it as a texture

   procedure Log_Graph (Object : in out Renderable_Graph; Default : Rendering.Framebuffers.Framebuffer);

   procedure Write_Graph
     (Object   : Renderable_Graph;
      Location : Resources.Locations.Writable_Location_Ptr;
      Path     : String);
   --  Write the frame graph as JSON to a file at the given path in the
   --  location
   --
   --  To pretty print the JSON in the file, execute:
   --
   --    python3 -m json.tool < graph.json

private

   No_Render_Pass : constant Render_Pass_Index := Render_Pass_Index'Last;
   No_Resource    : constant Handle_Type       := Handle_Type'Last;

   type Render_Pass
     (Frame_Graph : not null access Frame_Graphs.Frame_Graph) is tagged limited
   record
      Index : Render_Pass_Index;
   end record;

   type Render_Pass_Data is record
      Name        : Name_Strings.Bounded_String;
      Side_Effect : Boolean;

      State    : Rendering.States.State;
      Program  : Rendering.Programs.Program;
      Callback : Program_Callback_Access;

      Read_Offset, Write_Offset : Positive := 1;
      Read_Count, Write_Count   : Natural  := 0;

      Has_Depth   : Boolean := False;
      Has_Stencil : Boolean := False;
   end record;

   function Get_Next_ID return Natural;

   type Resource_ID is record
      Value : Natural := Get_Next_ID;
   end record;

   type Resource_Version is new Natural
     with Default_Value => 1;
   --  Version will be 0 if added as implicit input

   type Resource_Data is record
      Data           : Resource;
      Modified       : Boolean    := False;  --  True if there is a next version of this resource
      Implicit       : Boolean    := False;  --  Internally added for framebuffer attachments
      Input_Mode     : Read_Mode  := Not_Used;
      Output_Mode    : Write_Mode := Not_Used;
      Input_Binding  : Binding_Point := 0;
      Output_Binding : Binding_Point := 0;
      Render_Pass    : Render_Pass_Index := No_Render_Pass;
      --  The render pass that writes to this resource, No_Render_Pass if none
      Read_Count     : Natural := 0;
   end record;

   type Resource_Layer is new Natural;

   type Edge_Type is record
      Index : Handle_Type;
      Layer : Resource_Layer;
   end record;

   package Pass_Vectors     is new Containers.Bounded_Vectors (Render_Pass_Index, Render_Pass_Data);
   package Resource_Vectors is new Containers.Bounded_Vectors (Handle_Type, Resource_Data);
   package Handle_Vectors   is new Containers.Bounded_Vectors (Positive, Edge_Type);

   package Resource_Index_Vectors is new Containers.Bounded_Vectors (Positive, Handle_Type);
   package Pass_Index_Vectors is new Containers.Bounded_Vectors (Positive, Render_Pass_Index);

   type Frame_Graph
     (Maximum_Passes    : Render_Pass_Index;
      Maximum_Handles   : Positive;
      Maximum_Resources : Handle_Type) is tagged
   record
      Passes        : Pass_Vectors.Vector (Maximum_Passes);
      Resources     : Resource_Vectors.Vector (Maximum_Resources);
      --  Restriction 1: The method used by a render pass to read or write
      --  to a resource is stored in the resource itself. This means that
      --  a resource can only be read or written using one method.

      Read_Handles  : Handle_Vectors.Vector (Maximum_Handles);
      Write_Handles : Handle_Vectors.Vector (Maximum_Handles);
      --  An array of slices of handles to resources read or written by
      --  render passes. These describe the links between render passes
      --  and resources in the frame graph.
      --
      --  Each render pass has a slice of resources it reads and a slice of
      --  resources it writes. A slice is formed by a contiguous number of
      --  handles so that a render pass only needs to record the start
      --  offset and length of the slice.
      --
      --  Restriction 2: Because a slice is a contiguous number of handles,
      --  the recording of inputs and outputs of different render passes
      --  cannot be interleaved.
      --
      --  This restriction allows the graph to be implemented using just
      --  four simple arrays and the arrays should provide good data locality.

      Imported_Resources : Resource_Index_Vectors.Vector (Maximum_External_Resources);
      Exported_Resources : Resource_Index_Vectors.Vector (Maximum_External_Resources);
   end record;

   -----------------------------------------------------------------------------

   package Framebuffer_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Rendering.Framebuffers.Framebuffer, "=" => Rendering.Framebuffers."=");

   type Framebuffer_State is record
      Clear_Mask      : GL.Buffers.Buffer_Bits := (others => False);
      Invalidate_Mask : GL.Buffers.Buffer_Bits := (others => False);

      Clear_Buffers   : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
      Render_Buffers  : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
      Buffers_Equal   : Boolean;

      Invalidate_Points : Rendering.Framebuffers.Use_Point_Array := (others => False);

      Depth_Writes   : Boolean;
      Stencil_Writes : Boolean;
   end record;

   type Framebuffer_Pass is record
      Index       : Render_Pass_Index;
      Framebuffer : Framebuffer_Holders.Holder;
      State       : Framebuffer_State;
   end record;

   package Framebuffer_Pass_Vectors is new Containers.Bounded_Vectors
     (Render_Pass_Index, Framebuffer_Pass);

   type Present_Mode_Type is (Use_Default, Blit_To_Default, Render_To_Default);

   type Render_Pass_References_Array is array (Render_Pass_Index range <>) of Natural;
   type Resource_References_Array    is array (Handle_Type range <>) of Natural;

   type Render_Pass_Array is array (Render_Pass_Index range <>) of Render_Pass_Index;

   type Renderable_Graph
     (Maximum_Passes    : Render_Pass_Index;
      Maximum_Resources : Handle_Type;
      Graph             : not null access constant Frame_Graphs.Frame_Graph) is tagged limited
   record
      Framebuffers    : Framebuffer_Pass_Vectors.Vector (Maximum_Passes);
      --  Example with 2 passes + present pass:
      --
      --  case Present_Mode is         P1  P2      Present
      --     when Render_To_Default => FB1 FB2     DEFAULT
      --     when Blit_To_Default   => FB1 FB2     DEFAULT
      --     when Use_Default       => FB1 DEFAULT n/a

      Present_Mode        : Present_Mode_Type;
      Present_Render_Pass : Render_Pass_Data;  --  Program used when Present_Mode = Render_To_Default

      Last_FB_Index    : Render_Pass_Index;  --  Used when Present_Mode = Blit_To_Default
      Present_Resource : Handle_Type := No_Resource;

      Render_Pass_References : Render_Pass_References_Array (1 .. Maximum_Passes) := (others => 0);
      Resource_References    : Resource_References_Array (1 .. Maximum_Resources) := (others => 0);

      Pass_Order : Render_Pass_Array (1 .. Maximum_Passes);
      Pass_Count : Natural := 0;
      --  The ordering of the render passes depends on which resource is presented,
      --  with the pass writing to the presented resource being the last pass in the array
   end record;

end Orka.Frame_Graphs;
