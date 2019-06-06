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

with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Bounded;

with GL.Buffers;
with GL.Low_Level.Enums;
with GL.Pixels;

with Orka.Resources.Locations;

private with Orka.Containers.Bounded_Vectors;
private with Orka.Rendering.Framebuffers;

package Orka.Frame_Graphs is
   pragma Preelaborate;

   Maximum_Name_Length : constant := 16;

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Maximum_Name_Length);

   type Handle_Type is new Positive;

   type Resource_Version is private;

   type Extent_3D is record
      Width, Height, Depth : Natural := 0;
   end record;

   type Resource is record
      Name       : Name_Strings.Bounded_String;
      Kind       : GL.Low_Level.Enums.Texture_Kind;
      Format     : GL.Pixels.Internal_Format;
      Extent     : Extent_3D;
      Levels     : Positive := 1;
      Samples    : Positive := 1;
      Version    : Resource_Version;
   end record;

   function "+" (Value : String) return Name_Strings.Bounded_String;

   function "+" (Value : Name_Strings.Bounded_String) return String;

   ----------------------------------------------------------------------

   type Read_Mode is (Not_Used, Framebuffer_Attachment, Texture_Read, Image_Load);

   type Write_Mode is (Not_Used, Framebuffer_Attachment, Image_Store);

   type Input_Resource is record
      Mode : Read_Mode;
      Data : Resource;

      Written : Boolean;
      --  Written to by a previous render pass
   end record;

   type Output_Resource is record
      Mode : Write_Mode;
      Data : Resource;

      Read : Boolean;
      --  Read by a subsequent render pass
   end record;

   type Input_Resource_Array is array (Positive range <>) of Input_Resource;

   type Output_Resource_Array is array (Positive range <>) of Output_Resource;

   ----------------------------------------------------------------------

   type Render_Pass (<>) is tagged limited private;

   function Name (Object : Render_Pass) return String;

   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode);

   procedure Add_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Write   : Write_Mode);

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Write   : Write_Mode) return Resource
   with Pre => (if Read = Framebuffer_Attachment then Write = Framebuffer_Attachment);

   ----------------------------------------------------------------------

   type Builder
     (Maximum_Passes, Maximum_Handles : Positive;
      Maximum_Resources : Handle_Type) is tagged limited private;

   type Render_Pass_Data is private;

   function Name (Pass : Render_Pass_Data) return String;

   function Clear_Mask (Pass : Render_Pass_Data) return GL.Buffers.Buffer_Bits;

   function Invalidate_Mask (Pass : Render_Pass_Data) return GL.Buffers.Buffer_Bits;

   type Execute_Callback is access procedure (Pass : Render_Pass_Data);

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      Execute : not null Execute_Callback;
      Side_Effect : Boolean := False) return Render_Pass'Class
   with Pre => Name'Length <= Maximum_Name_Length;

   type Graph (<>) is tagged limited private;

   function Cull (Object : Builder) return Graph'Class;

   function Input_Resources
     (Object : Graph;
      Pass   : Render_Pass_Data) return Input_Resource_Array;

   function Output_Resources
     (Object : Graph;
      Pass   : Render_Pass_Data) return Output_Resource_Array;

   ----------------------------------------------------------------------

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

   procedure Render (Object : in out Graph);

private

   type Render_Pass (Frame_Graph : access Builder) is tagged limited record
      Index : Positive;
   end record;

   type Render_Pass_Data is record
      Name        : Name_Strings.Bounded_String;
      Execute     : Execute_Callback;

      Side_Effect : Boolean;
      References  : Natural := 0;
      Read_Offset, Write_Offset : Positive := 1;
      Read_Count, Write_Count   : Natural  := 0;

      Clear_Mask      : GL.Buffers.Buffer_Bits := (others => False);
      Invalidate_Mask : GL.Buffers.Buffer_Bits := (others => True);

      --  If Clear_Buffers = Render_Buffers then we only need to call
      --  Set_Draw_Buffers once in procedure Cull. If the buffers are
      --  different then a performance warning is logged
      Clear_Buffers   : GL.Buffers.Color_Buffer_List (0 .. 7)
        := (others => GL.Buffers.None);
      Render_Buffers  : GL.Buffers.Color_Buffer_List (0 .. 7)
        := (others => GL.Buffers.None);
      Buffers_Equal   : Boolean;
      --  TODO Use Draw_Buffer_Index as index type and drop some in Set_Draw_Buffers
   end record;

   type Resource_Version is record
      Version : Positive := 1;
   end record;

   type Resource_Data is record
      Description : Resource;
      Input_Mode  : Read_Mode  := Not_Used;
      Output_Mode : Write_Mode := Not_Used;
      Render_Pass : Natural := 0;
      Read_Count  : Natural := 0;
      References  : Natural := 0;
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
      Read_Handles  : Handle_Vectors.Vector (Maximum_Handles);
      Write_Handles : Handle_Vectors.Vector (Maximum_Handles);
   end record;

   -----------------------------------------------------------------------------

   package Framebuffer_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Rendering.Framebuffers.Framebuffer, "=" => Rendering.Framebuffers."=");

   type Framebuffer_Pass is record
      Index       : Positive;
      Framebuffer : Framebuffer_Holders.Holder;
   end record;

   package Framebuffer_Pass_Vectors is new Containers.Bounded_Vectors
     (Positive, Framebuffer_Pass);

   type Graph
     (Maximum_Passes, Maximum_Handles : Positive;
      Maximum_Resources : Handle_Type) is tagged limited
   record
      Graph        : Builder (Maximum_Passes, Maximum_Handles, Maximum_Resources);
      Framebuffers : Framebuffer_Pass_Vectors.Vector (Maximum_Passes);
   end record;

end Orka.Frame_Graphs;
