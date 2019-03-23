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

with Orka.Resources.Locations;

private with Orka.Containers.Bounded_Vectors;

package Orka.Frame_Graphs is
   pragma Preelaborate;

   Maximum_Name_Length : constant := 16;

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Maximum_Name_Length);

   type Resource_Version is private;

   type Resource is record
      Name       : Name_Strings.Bounded_String;
      Kind       : GL.Low_Level.Enums.Texture_Kind;
      Format     : GL.Pixels.Internal_Format;
      Levels     : Positive := 1;
      Samples    : Positive := 1;
      Version    : Resource_Version;
   end record;

   function "+" (Value : String) return Name_Strings.Bounded_String;

   ----------------------------------------------------------------------

   type Read_Mode is (Texture_Read, Image_Load);

   type Write_Mode is (Framebuffer_Attachment, Image_Store);

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
      Write   : Write_Mode) return Resource;

   ----------------------------------------------------------------------

   type Builder (Maximum_Passes, Maximum_Resources : Positive) is tagged limited private;

   type Render_Pass_Data is private;

   type Execute_Callback is access procedure (Pass : Render_Pass_Data);

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      Execute : not null Execute_Callback;
      Side_Effect : Boolean := False) return Render_Pass'Class
   with Pre => Name'Length <= Maximum_Name_Length;

   type Graph (<>) is tagged limited private;

   function Cull (Object : Builder) return Graph'Class;

   ----------------------------------------------------------------------

   procedure Write_Graph
     (Object   : Graph;
      Location : Resources.Locations.Writable_Location_Ptr;
      Path     : String);
   --  Write the frame graph as JSON to a file at the given path in the
   --  location
   --
   --  To pretty print the JSON in the file, execute:
   --
   --    python3 -m json.tool < graph.json

   procedure Render (Object : Graph);

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
   end record;

   type Resource_Version is record
      Version : Positive := 1;
   end record;

   type Extent_3D is record
      Width, Height, Depth : Natural := 0;
   end record;

   type Resource_Data is record
      Description : Resource;
      Extent      : Extent_3D;
      Render_Pass : Natural := 0;
      Read_Count  : Natural := 0;
      References  : Natural := 0;
   end record;

   package Pass_Vectors     is new Containers.Bounded_Vectors (Render_Pass_Data);
   package Resource_Vectors is new Containers.Bounded_Vectors (Resource_Data);
   package Handle_Vectors   is new Containers.Bounded_Vectors (Positive);

   type Builder (Maximum_Passes, Maximum_Resources : Positive) is tagged limited record
      Passes        : Pass_Vectors.Vector (Maximum_Passes);
      Resources     : Resource_Vectors.Vector (Maximum_Resources);
      Read_Handles  : Handle_Vectors.Vector (Maximum_Resources);
      Write_Handles : Handle_Vectors.Vector (Maximum_Resources);
   end record;

   type Graph (Maximum_Passes, Maximum_Resources : Positive) is tagged limited record
      Graph : Builder (Maximum_Passes, Maximum_Resources);
   end record;

end Orka.Frame_Graphs;
