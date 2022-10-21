--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with GL.Low_Level.Enums;
with GL.Pixels;

with Orka.Contexts;
with Orka.Resources.Locations;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.States;
with Orka.Resources.Locations;

private with Ada.Containers.Indefinite_Holders;

private with GL.Buffers;

private with Orka.Rendering.Programs;
private with Orka.Containers.Bounded_Vectors;

package Orka.Frame_Graphs is
   pragma Preelaborate;

   Maximum_Name_Length : constant := 16;

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Maximum_Name_Length);

   type Resource_Version is private;

   type Extent_3D is record
      Width, Height, Depth : Positive := 1;
   end record;

   type Resource is record
      Name       : Name_Strings.Bounded_String;
      Kind       : GL.Low_Level.Enums.Texture_Kind;
      Format     : GL.Pixels.Internal_Format;
      Extent     : Extent_3D;
      Levels     : Positive := 1;
      Samples    : Natural  := 0;
      Version    : Resource_Version;
   end record;

   function "+" (Value : String) return Name_Strings.Bounded_String;

   function "+" (Value : Name_Strings.Bounded_String) return String;

   ----------------------------------------------------------------------

   type Read_Mode is (Not_Used, Framebuffer_Attachment, Texture_Read, Image_Load);

   type Write_Mode is (Not_Used, Framebuffer_Attachment, Image_Store);

   type Binding_Point is new Natural;

   subtype Attachment_Point is Binding_Point range 0 .. 7;

   ----------------------------------------------------------------------

   type Render_Pass (<>) is tagged limited private;

   function Name (Object : Render_Pass) return String;

   --  TODO Does it make sense to add non-depth/stencil textures as FBO attachment?
   --  TODO Support adding a layer of a resource as an input?
   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Binding : Binding_Point)
   with Pre => (if Read = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an input to the render pass, so that it
   --  can be read as a texture, an image texture, or attached to the
   --  framebuffer

   procedure Add_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Write   : Write_Mode;
      Binding : Binding_Point)
   with Pre => (if Write = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an output to the render pass, so that it
   --  can be written as an image texture or attached to the framebuffer

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Write   : Write_Mode;
      Binding : Binding_Point) return Resource
   with Pre => not (Read = Framebuffer_Attachment xor Write = Framebuffer_Attachment)
                 and (if Write = Framebuffer_Attachment then Binding in Attachment_Point);
   --  Add the given resource as an input and an output to the render pass,
   --  indicating that the resource will be modified by the pass. The modified
   --  resource is returned and may be used as an input for other render passes.

   ----------------------------------------------------------------------

   type Render_Pass_Data is private;

   function Name (Pass : Render_Pass_Data) return String;

   ----------------------------------------------------------------------

   type Handle_Type is new Positive;

   type Builder
     (Maximum_Passes, Maximum_Handles : Positive;
      Maximum_Resources : Handle_Type) is tagged limited private;

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      State   : Rendering.States.State;
      Side_Effect : Boolean := False) return Render_Pass'Class
   with Pre => Name'Length <= Maximum_Name_Length;

   type Graph (<>) is tagged limited private;

   function Cull (Object : Builder; Present : Resource) return Graph'Class;

   ----------------------------------------------------------------------

   procedure Initialize
     (Object   : in out Graph;
      Location : Resources.Locations.Location_Ptr;
      Default  : Rendering.Framebuffers.Framebuffer)
   with Pre => Default.Default;

   procedure Render
     (Object  : in out Graph;
      Context : in out Contexts.Context'Class;
      Execute : access procedure (Pass : Render_Pass_Data));

   procedure Log_Graph (Object : in out Graph);

   procedure Write_Graph
     (Object   : in out Graph;
      Location : Resources.Locations.Writable_Location_Ptr;
      Path     : String);
   --  Write the frame graph as JSON to a file at the given path in the
   --  location
   --
   --  To pretty print the JSON in the file, execute:
   --
   --    python3 -m json.tool < graph.json

private

   type Render_Pass (Frame_Graph : access Builder) is tagged limited record
      Index : Positive;
   end record;

   type Render_Pass_Data is record
      Name        : Name_Strings.Bounded_String;

      Side_Effect : Boolean;
      Present     : Boolean;

      State : Rendering.States.State;

      References  : Natural := 0;
      Read_Offset, Write_Offset : Positive := 1;
      Read_Count, Write_Count   : Natural  := 0;

      Has_Depth   : Boolean := False;
      Has_Stencil : Boolean := False;
   end record;

   type Resource_Version is record
      Version : Natural := 1;  --  Version will be 0 if added as implicit input
   end record;

   type Resource_Data is record
      Description    : Resource;
      Modified       : Boolean    := False;  --  True if there is a next version of this resource
      Implicit       : Boolean    := False;  --  Internally added for framebuffer attachments
      Input_Mode     : Read_Mode  := Not_Used;
      Output_Mode    : Write_Mode := Not_Used;
      Input_Binding  : Binding_Point := 0;
      Output_Binding : Binding_Point := 0;
      Render_Pass    : Natural := 0;  --  The render pass that writes to this resource, 0 if none
      Read_Count     : Natural := 0;
      References     : Natural := 0;
   end record;

   package Pass_Vectors     is new Containers.Bounded_Vectors (Positive, Render_Pass_Data);
   package Resource_Vectors is new Containers.Bounded_Vectors (Handle_Type, Resource_Data);
   package Handle_Vectors   is new Containers.Bounded_Vectors (Positive, Handle_Type);

   type Builder
     (Maximum_Passes, Maximum_Handles : Positive;
      Maximum_Resources : Handle_Type) is tagged limited
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

      Present_Pass : Natural := 0;
   end record;

   -----------------------------------------------------------------------------

   package Framebuffer_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Rendering.Framebuffers.Framebuffer, "=" => Rendering.Framebuffers."=");

   type Framebuffer_Pass is record
      Index       : Positive;
      Framebuffer : Framebuffer_Holders.Holder;

      Clear_Mask      : GL.Buffers.Buffer_Bits;
      Invalidate_Mask : GL.Buffers.Buffer_Bits;

      Clear_Buffers   : GL.Buffers.Color_Buffer_List (0 .. 7);
      Render_Buffers  : GL.Buffers.Color_Buffer_List (0 .. 7);
      Buffers_Equal   : Boolean;

      Invalidate_Points : Rendering.Framebuffers.Use_Point_Array;

      Depth_Writes   : Boolean;
      Stencil_Writes : Boolean;
   end record;

   package Framebuffer_Pass_Vectors is new Containers.Bounded_Vectors
     (Positive, Framebuffer_Pass);

   type Present_Mode_Type is (Use_Default, Blit_To_Default, Render_To_Default);

   type Graph
     (Maximum_Passes, Maximum_Handles : Positive;
      Maximum_Resources : Handle_Type) is tagged limited
   record
      Graph           : Builder (Maximum_Passes, Maximum_Handles, Maximum_Resources);
      Framebuffers    : Framebuffer_Pass_Vectors.Vector (Maximum_Passes);
      Present_Mode    : Present_Mode_Type;
      Present_Program : Rendering.Programs.Program;
      Last_Pass_Index : Positive;
   end record;

end Orka.Frame_Graphs;
