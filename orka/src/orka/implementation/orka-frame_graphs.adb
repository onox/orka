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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with GL.Pixels;
with GL.Objects.Framebuffers;
with GL.Objects.Textures;
with GL.Toggles;
with GL.Types;

with Orka.Logging.Default;
with Orka.Rendering.Drawing;
with Orka.Rendering.Textures;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Strings;
with Orka.Types;

package body Orka.Frame_Graphs is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Renderer);

   function Trim_Image (Value : Integer) return String is
     (Orka.Strings.Trim (Integer'Image (Value)));

   function Trim_Image (Value : Extent_3D) return String is
     (Trim_Image (Value.Width) & " × " &
            Trim_Image (Value.Height) & " × " &
            Trim_Image (Value.Depth));

   function Trim_Image (Value : GL.Buffers.Buffer_Bits) return String is
      package SU renames Ada.Strings.Unbounded;

      Result : SU.Unbounded_String;
   begin
      if Value.Color then
         SU.Append (Result, " color");
      end if;
      if Value.Depth then
         SU.Append (Result, " depth");
      end if;
      if Value.Stencil then
         SU.Append (Result, " stencil");
      end if;
      return Orka.Strings.Trim (SU.To_String (Result));
   end Trim_Image;

   procedure Log_Resource (Value : Resource) is
   begin
      Log (Debug, "    " & (+Value.Name) & ":");
      Log (Debug, "      kind:     " & Value.Kind'Image);
      Log (Debug, "      format:   " & Value.Format'Image);
      Log (Debug, "      extent:   " & Trim_Image (Value.Extent));
      Log (Debug, "      samples:  " & Trim_Image (Value.Samples));
      Log (Debug, "      version:  " & Trim_Image (Value.Version.Version));
   end Log_Resource;

   package LE renames GL.Low_Level.Enums;

   function Name (Object : Resource_Data) return String is (+Object.Description.Name);

   procedure Find_Resource
     (Object  : in out Builder;
      Subject : Resource;
      Handle  : out Handle_Type;
      Found   : out Boolean)
   is
      use type Name_Strings.Bounded_String;
   begin
      Found := False;
      for Index in 1 .. Object.Resources.Length loop
         declare
            Description : Resource renames Object.Resources (Index).Description;
            Implicit    : Boolean  renames Object.Resources (Index).Implicit;
         begin
            if Subject.Name = Description.Name and Subject.Version = Description.Version
              and not Implicit
            then
               if Description /= Subject then
                  raise Constraint_Error with
                    "Already added different resource '" & (+Subject.Name) & "'";
               end if;

               Found  := True;
               Handle := Index;
               exit;
            end if;
         end;
      end loop;
   end Find_Resource;

   procedure Add_Resource
     (Object  : in out Builder;
      Subject : Resource;
      Handle  : out Handle_Type)
   is
      Found : Boolean;
   begin
      Object.Find_Resource (Subject, Handle, Found);

      if not Found then
         Object.Resources.Append
           ((Description => Subject,
             others      => <>));
         Handle := Object.Resources.Length;
      end if;
   end Add_Resource;

   -----------------------------------------------------------------------------

   type Input_Resource is record
      Mode    : Read_Mode;
      Data    : Resource;
      Binding : Binding_Point;

      Implicit : Boolean;
      Written  : Boolean;
      --  Written to by a previous render pass
   end record;

   type Output_Resource is record
      Mode    : Write_Mode;
      Data    : Resource;
      Binding : Binding_Point;

      Implicit : Boolean;
      Read     : Boolean;
      --  Read by a subsequent render pass
   end record;

   type Input_Resource_Array is array (Positive range <>) of Input_Resource;

   type Output_Resource_Array is array (Positive range <>) of Output_Resource;

   -----------------------------------------------------------------------------

   subtype Attachment_Format is Orka.Rendering.Textures.Format_Kind;

   use all type Attachment_Format;

   procedure Verify_Depth_Stencil
     (Pass   : Render_Pass_Data;
      Format : Attachment_Format) is
   begin
      if Pass.Has_Depth and then Format in Depth_Stencil | Depth then
         raise Program_Error with
           "Render pass '" & Name (Pass) & "' already has a depth attachment";
      end if;

      if Pass.Has_Stencil and then Format in Depth_Stencil | Stencil then
         raise Program_Error with
           "Render pass '" & Name (Pass) & "' already has a stencil attachment";
      end if;
   end Verify_Depth_Stencil;

   procedure Set_Depth_Stencil
     (Pass   : in out Render_Pass_Data;
      Format : Attachment_Format) is
   begin
      if Format in Depth_Stencil | Depth then
         Pass.Has_Depth := True;
      end if;
      if Format in Depth_Stencil | Stencil then
         Pass.Has_Stencil := True;
      end if;
   end Set_Depth_Stencil;

   function Get_Attachment_Format
     (Format : GL.Pixels.Internal_Format) return Attachment_Format
   renames Orka.Rendering.Textures.Get_Format_Kind;

   -----------------------------------------------------------------------------

   use type GL.Objects.Textures.Texture;

   package Texture_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => GL.Objects.Textures.Texture,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Textures : Texture_Maps.Map;

   function Get_Texture (Subject : Resource) return GL.Objects.Textures.Texture
     with Post => Get_Texture'Result.Allocated is
   begin
      return Textures.Element (+Subject.Name);
   exception
      when Constraint_Error =>
         declare
            Result : GL.Objects.Textures.Texture (Subject.Kind);
         begin
            Result.Allocate_Storage
              (Levels  => Size (Subject.Levels),
               Samples => Size (Subject.Samples),
               Format  => Subject.Format,
               Width   => Size (Subject.Extent.Width),
               Height  => Size (Subject.Extent.Height),
               Depth   => Size (Subject.Extent.Depth));
            Textures.Insert (+Subject.Name, Result);
            return Result;
         end;
   end Get_Texture;

   -----------------------------------------------------------------------------

   function "+" (Value : Name_Strings.Bounded_String) return String is
     (Name_Strings.To_String (Value));

   function "+" (Value : String) return Name_Strings.Bounded_String is
     (Name_Strings.To_Bounded_String (Value));

   function Name (Object : Render_Pass) return String is
     (+Object.Frame_Graph.Passes (Object.Index).Name);

   function Name (Pass : Render_Pass_Data) return String is (+Pass.Name);

   -----------------------------------------------------------------------------

   procedure Add_Input
     (Object   : Render_Pass;
      Subject  : Resource;
      Read     : Read_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean);

   function Add_Pass
     (Object : in out Builder;
      Name   : String;
      State  : Rendering.States.State;
      Side_Effect, Present : Boolean) return Render_Pass'Class is
   begin
      Object.Passes.Append
        ((Name        => +Name,
          Side_Effect => Side_Effect,
          Present     => Present,
          State       => State,
          others      => <>));
      return Render_Pass'
        (Frame_Graph => Object'Access,
         Index       => Object.Passes.Length);
   end Add_Pass;

   function Add_Pass
     (Object : in out Builder;
      Name   : String;
      State  : Rendering.States.State;
      Side_Effect : Boolean := False) return Render_Pass'Class
   is (Object.Add_Pass (Name, State, Side_Effect => Side_Effect, Present => False));

   procedure Add_Present
     (Object  : in out Builder;
      Subject : Resource;
      Handle  : out Handle_Type)
   is
      Found : Boolean;
   begin
      Object.Find_Resource (Subject, Handle, Found);

      if not Found then
         raise Constraint_Error with "Presented resource not found in graph";
      end if;

      declare
         Pass : constant Render_Pass'Class := Object.Add_Pass
           ("Present", (others => <>), Side_Effect => True, Present => True);
         Resource : Resource_Data renames Object.Resources (Handle);
      begin
         --  The present pass does not always read the resource: the previous
         --  render pass will use the default framebuffer to write to the
         --  resource (mode 1), or the present pass will blit (mode 2) or
         --  render (mode 3) the resource to the default framebuffer
         Render_Pass (Pass).Add_Input (Subject, Texture_Read, 0, Handle, Implicit => False);

         Object.Present_Pass := Pass.Index;
      end;
   end Add_Present;

   -----------------------------------------------------------------------------

   procedure Add_Output
     (Object   : Render_Pass;
      Subject  : Resource;
      Write    : Write_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean)
   is
      Graph : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
      Pass  : Render_Pass_Data renames Graph.Passes (Object.Index);
      Attachment : constant Attachment_Format := Get_Attachment_Format (Subject.Format);
   begin
      if Pass.Write_Count > 0 and then
         Pass.Write_Offset + Pass.Write_Count /= Graph.Write_Handles.Length + 1
      then
         raise Program_Error with "Cannot interleave Add_Output calls for different passes";
      end if;

      if Implicit and Write = Framebuffer_Attachment then
         Verify_Depth_Stencil (Pass, Attachment);

         declare
            Handle : Handle_Type;
            Prev_Subject : Resource := Subject;
         begin
            Prev_Subject.Version.Version := Subject.Version.Version - 1;
            Object.Add_Input
              (Prev_Subject, Framebuffer_Attachment, Binding, Handle, Implicit => False);
            Graph.Resources (Handle).Implicit := True;
         end;
      end if;

      Add_Resource (Graph, Subject, Handle);

      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         if Resource.Render_Pass /= 0 then
            raise Constraint_Error with "Resource '" & Name (Resource) & "'" &
              " already written by pass '" & (+Graph.Passes (Resource.Render_Pass).Name) & "'";
         end if;

         pragma Assert (Resource.Output_Mode = Not_Used);

         Resource.Render_Pass    := Object.Index;
         Resource.Output_Mode    := Write;
         Resource.Output_Binding := Binding;
      end;

      --  Register resource as 'written' by the render pass
      Graph.Write_Handles.Append (Handle);

      if Pass.Write_Count = 0 then
         Pass.Write_Offset := Graph.Write_Handles.Length;
      end if;
      Pass.Write_Count := Pass.Write_Count + 1;

      Set_Depth_Stencil (Pass, Attachment);
   end Add_Output;

   procedure Add_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Write   : Write_Mode;
      Binding : Binding_Point)
   is
      Handle : Handle_Type;
   begin
      Object.Add_Output (Subject, Write, Binding, Handle, Implicit => True);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Output_Mode = Write);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Output_Binding = Binding);
   end Add_Output;

   procedure Add_Input
     (Object   : Render_Pass;
      Subject  : Resource;
      Read     : Read_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean)
   is
      Graph : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
      Pass  : Render_Pass_Data renames Graph.Passes (Object.Index);

      Attachment : constant Attachment_Format := Get_Attachment_Format (Subject.Format);
   begin
      if Pass.Read_Count > 0 and then
        Pass.Read_Offset + Pass.Read_Count /= Graph.Read_Handles.Length + 1
      then
         raise Program_Error with "Cannot interleave Add_Input calls for different passes";
      end if;

      if Implicit and Read = Framebuffer_Attachment then
         if Attachment = Color then
            raise Program_Error with "Use Add_Output or Add_Input_Output for color resource";
         end if;

         Verify_Depth_Stencil (Pass, Attachment);

         declare
            Handle : Handle_Type;
            Next_Subject : Resource := Subject;
         begin
            Next_Subject.Version.Version := Subject.Version.Version + 1;
            Object.Add_Output
              (Next_Subject, Framebuffer_Attachment, Binding, Handle, Implicit => False);
            Graph.Resources (Handle).Implicit := True;
         end;
      end if;

      Add_Resource (Graph, Subject, Handle);

      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         --  Resource is not considered modified if the next version is implicit
         if Resource.Modified then
            raise Constraint_Error with
              "Cannot read old version of modified resource '" & Name (Resource) & "'";
         end if;

         --  Because a resource records only one input mode, verify this
         --  resource is read by multiple render passes using only one
         --  particular method
         if Resource.Input_Mode /= Not_Used and Resource.Input_Mode /= Read then
            raise Constraint_Error with
              "Resource '" & Name (Resource) & "' must be read as " & Resource.Input_Mode'Image;
         end if;

         Resource.Read_Count    := Resource.Read_Count + 1;
         Resource.Input_Mode    := Read;
         Resource.Input_Binding := Binding;
      end;

      --  Register resource as 'read' by the render pass
      Graph.Read_Handles.Append (Handle);

      if Pass.Read_Count = 0 then
         Pass.Read_Offset := Graph.Read_Handles.Length;
      end if;
      Pass.Read_Count := Pass.Read_Count + 1;

      Set_Depth_Stencil (Pass, Attachment);
   end Add_Input;

   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Binding : Binding_Point)
   is
      Handle : Handle_Type;
   begin
      Object.Add_Input (Subject, Read, Binding, Handle, Implicit => True);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Input_Mode = Read);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Input_Binding = Binding);
   end Add_Input;

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Write   : Write_Mode;
      Binding : Binding_Point) return Resource
   is
      Graph  : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
      Handle : Handle_Type;

      Next_Subject : Resource := Subject;
   begin
      Object.Add_Input (Subject, Read, Binding, Handle, Implicit => False);
      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         Resource.Modified := True;
      end;

      Next_Subject.Version.Version := Subject.Version.Version + 1;
      Object.Add_Output (Next_Subject, Write, Binding, Handle, Implicit => False);
      return Next_Subject;
   end Add_Input_Output;

   function Cull (Object : Builder; Present : Resource) return Graph'Class is
      Stack : Handle_Vectors.Vector (Positive (Object.Resources.Length));
      Index : Handle_Type;
   begin
      return Result : Graph
        (Maximum_Passes    => Object.Maximum_Passes,
         Maximum_Handles   => Object.Maximum_Handles,
         Maximum_Resources => Object.Maximum_Resources)
      do
         --  Copy data structures before culling
         Result.Graph.Passes        := Object.Passes;
         Result.Graph.Resources     := Object.Resources;
         Result.Graph.Read_Handles  := Object.Read_Handles;
         Result.Graph.Write_Handles := Object.Write_Handles;

         Add_Present (Result.Graph, Present, Index);
         declare
            Resource : Resource_Data renames Result.Graph.Resources (Index);
         begin
            if Resource.Render_Pass = 0 then
               raise Constraint_Error with "Presented resource not written by a pass";
            end if;
         end;

         --  Raise an error if there is a render pass that will be
         --  culled immediately. This simplifies the stack so that it
         --  only needs to contain indices of resources.
         for Pass of Result.Graph.Passes loop
            Pass.References := Pass.Write_Count;

            if not Pass.Side_Effect and Pass.References = 0 then
               raise Constraint_Error with
                 "Render pass '" & (+Pass.Name) & "' does not write to any resource";
            end if;
         end loop;

         for Index in 1 .. Result.Graph.Resources.Length loop
            declare
               Resource : Resource_Data renames Result.Graph.Resources (Index);
            begin
               Resource.References := Resource.Read_Count;

               if Resource.References = 0 then
                  if Resource.Render_Pass = 0 then
                     raise Constraint_Error with
                       "Resource '" & Name (Resource) & " not connected";
                  end if;
                  Stack.Append (Index);
               end if;
            end;
         end loop;

         while not Stack.Is_Empty loop
            Stack.Remove_Last (Index);

            declare
               --  Pass is the render pass that writes to the resource
               Resource : Resource_Data renames Result.Graph.Resources (Index);
               Pass : Render_Pass_Data renames Result.Graph.Passes (Resource.Render_Pass);

               --  Assert that the render pass does write to the resource
               Write_Offset : Positive renames Pass.Write_Offset;
               Write_Count  : Natural  renames Pass.Write_Count;
               pragma Assert
                 (for some Offset in Write_Offset .. Write_Offset + Write_Count - 1 =>
                    Result.Graph.Write_Handles (Offset) = Index);
            begin
               Pass.References := Pass.References - 1;

               --  Update ref count of resources read by the render pass
               --  if the render pass got culled
               if not Pass.Side_Effect and Pass.References = 0 then
                  for Index in Pass.Read_Offset .. Pass.Read_Offset + Pass.Read_Count - 1 loop
                     declare
                        Resource_Handle : constant Handle_Type :=
                          Result.Graph.Read_Handles (Index);
                        Resource : Resource_Data renames
                          Result.Graph.Resources (Resource_Handle);
                     begin
                        Resource.References := Resource.References - 1;

                        if Resource.References = 0 and Resource.Render_Pass /= 0 then
                           Stack.Append (Resource_Handle);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;

         --  Here we are done with culling. Next step is to iterate over
         --  the passes that survived culling and create framebuffers and
         --  textures.
      end return;
   end Cull;

   function Input_Resources
     (Object : Graph;
      Pass   : Render_Pass_Data) return Input_Resource_Array is
   begin
      return Result : Input_Resource_Array (1 .. Pass.Read_Count) do
         for Index in 1 .. Pass.Read_Count loop
            declare
               Data : Resource_Data renames Object.Graph.Resources
                 (Object.Graph.Read_Handles (Pass.Read_Offset + Index - 1));
            begin
               Result (Index) := (Mode     => Data.Input_Mode,
                                  Data     => Data.Description,
                                  Binding  => Data.Input_Binding,
                                  Written  => Data.Render_Pass /= 0,
                                  Implicit => Data.Implicit);
            end;
         end loop;
      end return;
   end Input_Resources;

   function Output_Resources
     (Object : Graph;
      Pass   : Render_Pass_Data) return Output_Resource_Array is
   begin
      return Result : Output_Resource_Array (1 .. Pass.Write_Count) do
         for Index in 1 .. Pass.Write_Count loop
            declare
               Data : Resource_Data renames Object.Graph.Resources
                 (Object.Graph.Write_Handles (Pass.Write_Offset + Index - 1));
            begin
               Result (Index) := (Mode     => Data.Output_Mode,
                                  Data     => Data.Description,
                                  Binding  => Data.Output_Binding,
                                  Read     => Data.References > 0,
                                  Implicit => Data.Implicit);
            end;
         end loop;
      end return;
   end Output_Resources;

   procedure Initialize
     (Object   : in out Graph;
      Location : Resources.Locations.Location_Ptr;
      Default  : Rendering.Framebuffers.Framebuffer)
   is
      subtype Selector_Type is GL.Buffers.Explicit_Color_Buffer_Selector;
      subtype Buffer_Type   is GL.Buffers.Draw_Buffer_Index;
      subtype Point_Type    is Rendering.Framebuffers.Color_Attachment_Point;

      function To_Color_Buffer (Value : Attachment_Point) return Selector_Type is
        (Selector_Type'Val (Selector_Type'Pos (Selector_Type'First) + Value));

      function To_Attachment_Point (Value : Attachment_Point) return Point_Type is
        (Point_Type'Val (Point_Type'Pos (Point_Type'First) + Value));

      use type LE.Texture_Kind;

      Present_Pass : Render_Pass_Data renames Object.Graph.Passes (Object.Graph.Present_Pass);
      pragma Assert (Present_Pass.Read_Count = 1);

      --  Look up the resource that is going to be presented to the screen
      Present_Resource_Handle : constant Handle_Type
        := Object.Graph.Read_Handles (Present_Pass.Read_Offset);
      Present_Resource : Resource_Data renames Object.Graph.Resources (Present_Resource_Handle);

      Default_Extent : constant Extent_3D :=
        (Natural (Default.Width), Natural (Default.Height), 1);

      Last_Pass_Index : constant Positive := Present_Resource.Render_Pass;
   begin
      declare
         package Programs renames Orka.Rendering.Programs;

         Resource : Resource_Data renames Present_Resource;

         Format : constant Attachment_Format :=
           Get_Attachment_Format (Resource.Description.Format);

         --  Look up the last render pass *before* the present pass
         Last_Render_Pass : Render_Pass_Data renames Object.Graph.Passes (Last_Pass_Index);

         Attachments : Natural := 0;
         Has_Non_Color_Attachment : Boolean := False;
      begin
         --  Mode 1: Previous render pass has one FB color attachment (this resource)
         --    Action: Previous render pass can use the default framebuffer
         --
         --  Mode 2: Previous render pass has multiple FB color attachments
         --          or other non-color FB attachments
         --    Action: Blit the resource to the default framebuffer
         --
         --  Mode 3: Resource is not written as a FB color attachment
         --          or resource is a depth and/or stencil attachment
         --    Action: Bind the resource as a texture and render to the
         --            default framebuffer
         if Resource.Output_Mode /= Framebuffer_Attachment or Format /= Color then
            --  Mode 3: Bind resource as texture and render to default framebuffer
            --  (Presented resource is added as input to present pass in procedure Add_Present)
            Object.Present_Mode := Render_To_Default;

            Object.Present_Program := Programs.Create_Program (Programs.Modules.Create_Module
              (Location,
               VS => "oversized-triangle.vert",
               FS => "frame-graph-present.frag"));

            Object.Present_Program.Uniform ("screenResolution").Set_Vector
              (Orka.Types.Singles.Vector4'
                (Orka.Float_32 (Default.Width), Orka.Float_32 (Default.Height), 0.0, 0.0));

            Log (Warning, "Presenting " & Name (Resource) & " using extra render pass");

            if Resource.Output_Mode /= Framebuffer_Attachment then
               Log (Warning, "  output mode: " & Resource.Output_Mode'Image &
                 " (/= " & Read_Mode'Image (Framebuffer_Attachment) & ")");
            end if;

            if Format /= Color then
               Log (Warning, "  format: " & Format'Image &
                 " (/= " & Color'Image & ")");
            end if;
         else
            for Last_Resource of Object.Output_Resources (Last_Render_Pass) loop
               if Last_Resource.Mode = Framebuffer_Attachment then
                  case Get_Attachment_Format (Last_Resource.Data.Format) is
                     when Color =>
                        Attachments := Attachments + 1;
                     when Depth | Depth_Stencil | Stencil =>
                        --  Cannot use mode 1, because we have no control over the format
                        --  of the depth/stencil buffer of the default framebuffer
                        Has_Non_Color_Attachment := True;
                  end case;
               end if;
            end loop;

            if Attachments = 1 and not Has_Non_Color_Attachment
              and Resource.Description.Kind = LE.Texture_2D
              and Resource.Description.Extent = Default_Extent
              and Resource.Description.Samples = Natural (Default.Samples)
            then
               --  Mode 1: Use Default as the framebuffer of Last_Render_Pass
               Object.Present_Mode := Use_Default;

               Log (Debug, "Presenting " & Name (Resource) & " using default framebuffer");
            elsif Resource.Description.Kind in LE.Texture_2D | LE.Texture_2D_Multisample then
               --  Mode 2
               Object.Present_Mode := Blit_To_Default;

               Log (Warning, "Presenting " & Name (Resource) & " by blitting");

               if Attachments /= 1 or Has_Non_Color_Attachment then
                  Log (Warning, "  last render pass:");
                  Log (Warning, "    name:        " & Name (Last_Render_Pass));
                  if Attachments /= 1 then
                     Log (Warning, "    attachments: " & Trim_Image (Attachments) & " (/= 1)");
                  end if;
                  if Has_Non_Color_Attachment then
                     Log (Warning, "    has depth or stencil attachments");
                  end if;
               end if;

               if Resource.Description.Kind /= LE.Texture_2D then
                  Log (Warning, "  kind: " & Resource.Description.Kind'Image &
                    " (/= " & LE.Texture_2D'Image & ")");
               end if;

               if Resource.Description.Extent /= Default_Extent then
                  Log (Warning, "  scaling: from " &
                    Trim_Image (Resource.Description.Extent) & " to " &
                    Trim_Image (Default_Extent));
               end if;

               if Resource.Description.Samples /= Natural (Default.Samples) then
                  Log (Warning, "  samples: " &
                    Trim_Image (Resource.Description.Samples) & " (/= " &
                    Trim_Image (Natural (Default.Samples)) & ")");
               end if;
            end if;
         end if;
      end;

      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
            Width, Height, Samples_Attachments : Natural := Natural'First;

            Clear_Mask      : GL.Buffers.Buffer_Bits := (others => False);
            Invalidate_Mask : GL.Buffers.Buffer_Bits := (others => False);

            --  If Clear_Buffers = Render_Buffers then we only need to call
            --  Set_Draw_Buffers once after creating the framebuffer, otherwise
            --  buffers need to be set before clearing and rendering
            Clear_Buffers  : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
            Render_Buffers : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
            Buffers_Equal  : Boolean := False;

            Invalidate_Points : Rendering.Framebuffers.Use_Point_Array := (others => False);

            Depth_Writes, Stencil_Writes : Boolean := True;

            Clear_Buffer, Invalidate_Buffer : Boolean;
         begin
            if Pass.Side_Effect or Pass.References > 0 then
               --  Clear input resources read as framebuffer attachments
               --  which are new (not written by a previous render pass)
               for Resource of Object.Input_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Clear_Buffer := not Resource.Written;

                     case Get_Attachment_Format (Resource.Data.Format) is
                        when Depth_Stencil =>
                           Clear_Mask.Depth   := Clear_Buffer;
                           Clear_Mask.Stencil := Clear_Buffer;
                        when Depth =>
                           Clear_Mask.Depth   := Clear_Buffer;
                        when Stencil =>
                           Clear_Mask.Stencil := Clear_Buffer;
                        when Color =>
                           --  Clear the color buffers, but only those that
                           --  do not have a render pass that writes to it
                           if Clear_Buffer then
                              Clear_Buffers (Buffer_Type (Resource.Binding)) :=
                                To_Color_Buffer (Resource.Binding);
                              Clear_Mask.Color := True;
                           end if;
                     end case;
                  end if;
               end loop;

               --  Present pass is always the default framebuffer. Make sure
               --  the color buffer of the default framebuffer is cleared when
               --  we manually need to blit or render some texture to the screen
               if Pass.Present then
                  Clear_Mask.Color := True;
               end if;

               --  Invalidate output attachments that are transcient
               --  (not read by a subsequent render pass)
               for Resource of Object.Output_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Invalidate_Buffer := not Resource.Read;

                     Samples_Attachments :=
                       Natural'Max (Samples_Attachments, Resource.Data.Samples);

                     case Get_Attachment_Format (Resource.Data.Format) is
                        when Depth_Stencil =>
                           Invalidate_Mask.Depth   := Invalidate_Buffer;
                           Invalidate_Mask.Stencil := Invalidate_Buffer;

                           Depth_Writes   := not Resource.Implicit;
                           Stencil_Writes := not Resource.Implicit;
                        when Depth =>
                           Invalidate_Mask.Depth   := Invalidate_Buffer;

                           Depth_Writes   := not Resource.Implicit;
                        when Stencil =>
                           Invalidate_Mask.Stencil := Invalidate_Buffer;

                           Stencil_Writes := not Resource.Implicit;
                        when Color =>
                           --  Invalidate the color buffers, but only those that
                           --  are not read by a subsequent render pass
                           if Invalidate_Buffer then
                              Invalidate_Points (To_Attachment_Point (Resource.Binding)) := True;
                              Invalidate_Mask.Color := True;
                           end if;

                           --  Even if the resource is not read, the buffer is
                           --  still used as a draw buffer because the shader
                           --  will probably render to it
                           Render_Buffers (Buffer_Type (Resource.Binding)) :=
                             To_Color_Buffer (Resource.Binding);
                     end case;
                  end if;

                  --  Compute maximum width and height over all the output
                  --  resources (which may be attached to the framebuffer).
                  --  Ideally all attachments have the same extent, but the
                  --  GL spec allows for them to be different. Furthermore,
                  --  a framebuffer may have zero attachments, so iterate over
                  --  all resources irrespective of their write mode.
                  Width  := Natural'Max (Width, Resource.Data.Extent.Width);
                  Height := Natural'Max (Height, Resource.Data.Extent.Height);
               end loop;

               if Pass.Write_Count = 0 then
                  pragma Assert (Pass.Side_Effect);

                  --  Use width and height from default framebuffer
                  Width  := Natural (Default.Width);
                  Height := Natural (Default.Height);
               else
                  pragma Assert (Width > 0 and Height > 0);
               end if;

               declare
                  use type GL.Buffers.Color_Buffer_List;
               begin
                  --  Output attachments always have a corresponding
                  --  input resource (implicit if necessary), but if
                  --  the resource is not new (written by previous pass)
                  --  then the resource is not cleared
                  Buffers_Equal := Clear_Buffers = Render_Buffers;
               end;

               --  Skip present pass if last normal render pass already uses default framebuffer
               if Object.Present_Mode /= Use_Default or not Pass.Present then
                  Object.Framebuffers.Append
                    ((Index       => Index,
                      Framebuffer => Framebuffer_Holders.To_Holder
                        (if Object.Present_Mode = Use_Default and Last_Pass_Index = Index then
                           Default
                         elsif Object.Present_Mode /= Use_Default and Pass.Present then
                           Default
                         else
                           Rendering.Framebuffers.Create_Framebuffer
                             (Width   => Size (Width),
                              Height  => Size (Height),
                              Samples => Size (Samples_Attachments))),

                      Clear_Mask        => Clear_Mask,
                      Invalidate_Mask   => Invalidate_Mask,

                      Clear_Buffers     => Clear_Buffers,
                      Render_Buffers    => Render_Buffers,
                      Buffers_Equal     => Buffers_Equal,

                      Invalidate_Points => Invalidate_Points,

                      Depth_Writes      => Depth_Writes,
                      Stencil_Writes    => Stencil_Writes));

                  if Object.Present_Mode = Use_Default and Last_Pass_Index = Index then
                     Pass.Side_Effect := True;
                     Pass.Present     := True;
                  elsif Object.Present_Mode = Blit_To_Default and Last_Pass_Index = Index then
                     Object.Last_Pass_Index := Object.Framebuffers.Length;
                  end if;

                  declare
                     procedure Set_Buffers
                       (Framebuffer : in out Rendering.Framebuffers.Framebuffer)
                     is
                        use type GL.Buffers.Color_Buffer_Selector;
                     begin
                        --  Clear color to black and depth to 0.0 (because of reversed Z)
                        Framebuffer.Set_Default_Values
                          ((Color => (0.0, 0.0, 0.0, 1.0), Depth => 0.0, others => <>));

                        if Framebuffer.Default then
                           --  The resource is 'read' by the present pass
                           pragma Assert (not Invalidate_Mask.Color);

                           if not Buffers_Equal then
                              pragma Assert
                                (for all Buffer of Clear_Buffers => Buffer = GL.Buffers.None);
                           end if;

                           if Object.Present_Mode = Use_Default then
                              pragma Assert (Render_Buffers
                                (Render_Buffers'First) = GL.Buffers.Color_Attachment0);
                              pragma Assert
                                (for all Index in Render_Buffers'First + 1 .. Render_Buffers'Last
                                   => Render_Buffers (Index) = GL.Buffers.None);
                              --  Not calling Set_Draw_Buffers because the default
                              --  framebuffer already has an initial draw buffer
                              --  (GL.Buffers.Back_Left for double-buffered context)
                           end if;
                        else
                           if Object.Present_Mode = Blit_To_Default
                             and Last_Pass_Index = Index
                           then
                              --  Compute the point of the resource read by the present pass
                              for Resource of Object.Output_Resources (Pass) loop
                                 if Resource.Mode = Framebuffer_Attachment
                                    and Get_Attachment_Format (Resource.Data.Format) = Color
                                    and Resource.Data = Present_Resource.Description
                                 then
                                    Framebuffer.Set_Read_Buffer
                                      (To_Color_Buffer (Resource.Binding));
                                 end if;
                              end loop;
                           end if;

                           if Buffers_Equal then
                              Framebuffer.Set_Draw_Buffers (Render_Buffers);
                           end if;

                           for Resource of Object.Input_Resources (Pass) loop
                              if Resource.Mode = Framebuffer_Attachment then
                                 --  TODO Support attaching layer of resource
                                 --       using value in 1 .. Resource.Data.Extent.Depth
                                 if Get_Attachment_Format (Resource.Data.Format) /= Color then
                                    Framebuffer.Attach (Get_Texture (Resource.Data));
                                 else
                                    Framebuffer.Attach
                                      (Texture    => Get_Texture (Resource.Data),
                                       Attachment => To_Attachment_Point (Resource.Binding));
                                 end if;
                              end if;
                           end loop;
                        end if;
                     end Set_Buffers;
                  begin
                     Object.Framebuffers (Object.Framebuffers.Length).Framebuffer.Update_Element
                       (Set_Buffers'Access);
                  end;
               end if;
            end if;
         end;
      end loop;
   end Initialize;

   procedure Log_Graph (Object : in out Graph) is
   begin
      for Data of Object.Framebuffers loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Data.Index);
            pragma Assert (Pass.Side_Effect or else Pass.References > 0);

            procedure Execute_Pass (Framebuffer : Rendering.Framebuffers.Framebuffer) is
               use type GL.Buffers.Explicit_Color_Buffer_Selector;
            begin
               Log (Debug, "Pass " & (+Pass.Name) & " (" & Trim_Image (Data.Index) & ")");
               Log (Debug, "  count:");
               Log (Debug, "    read:  " & Pass.Read_Count'Image);
               Log (Debug, "    write: " & Pass.Write_Count'Image);
               Log (Debug, "  references:   " & Pass.References'Image);
               Log (Debug, "  side effects: " & Pass.Side_Effect'Image);
               Log (Debug, "  present:      " & Pass.Present'Image);

               Log (Debug, "  framebuffer: " & Framebuffer.Image);
               Log (Debug, "    writes:");
               Log (Debug, "      depth:   " & Data.Depth_Writes'Image);
               Log (Debug, "      stencil: " & Data.Stencil_Writes'Image);
               Log (Debug, "    masks:");
               Log (Debug, "      clear:      " & Trim_Image (Data.Clear_Mask));
               Log (Debug, "      invalidate: " & Trim_Image (Data.Invalidate_Mask));

               if not Framebuffer.Default then
                  Log (Debug, "    invalidate:");
                  for I in Data.Invalidate_Points'Range loop
                     if Data.Invalidate_Points (I) then
                        Log (Debug, "    - " & I'Image);
                     end if;
                  end loop;
                  Log (Debug, "    buffers:");
                  Log (Debug, "      clear:");
                  for I in Data.Clear_Buffers'Range loop
                     if Data.Clear_Buffers (I) /= GL.Buffers.None then
                        Log (Debug, "        - " & Data.Clear_Buffers (I)'Image);
                     end if;
                  end loop;
                  if not Data.Buffers_Equal then
                     Log (Debug, "      render:");
                     for I in Data.Render_Buffers'Range loop
                        if Data.Clear_Buffers (I) /= GL.Buffers.None then
                           Log (Debug, "        - " & Data.Render_Buffers (I)'Image);
                        end if;
                     end loop;
                  end if;
               end if;

               Log (Debug, "  inputs:");
               for Resource of Object.Input_Resources (Pass) loop
                  Log_Resource (Resource.Data);
                  Log (Debug, "      mode:     " & Resource.Mode'Image);
                  Log (Debug, "      binding:  " & Trim_Image (Natural (Resource.Binding)));
                  Log (Debug, "      implicit: " & Resource.Implicit'Image);
                  Log (Debug, "      written:  " & Resource.Written'Image);
               end loop;

               Log (Debug, "  outputs:");
               for Resource of Object.Output_Resources (Pass) loop
                  Log_Resource (Resource.Data);
                  Log (Debug, "      mode:     " & Resource.Mode'Image);
                  Log (Debug, "      binding:  " & Trim_Image (Natural (Resource.Binding)));
                  Log (Debug, "      implicit: " & Resource.Implicit'Image);
                  Log (Debug, "      read:     " & Resource.Read'Image);
               end loop;

               if Pass.Present then
                  Log (Debug, "Present pass mode: " & Object.Present_Mode'Image);

                  case Object.Present_Mode is
                     when Blit_To_Default =>
                        Log (Debug, "Blitting to default framebuffer from pass " &
                          Trim_Image (Object.Framebuffers (Object.Last_Pass_Index).Index));
                     when Use_Default | Render_To_Default =>
                        null;
                  end case;
               end if;
            end Execute_Pass;
         begin
            Data.Framebuffer.Query_Element (Execute_Pass'Access);
         end;
      end loop;
   end Log_Graph;

   procedure Render
     (Object  : in out Graph;
      Context : in out Contexts.Context'Class;
      Execute : access procedure (Pass : Render_Pass_Data))
   is
      package Textures renames Orka.Rendering.Textures;
   begin
      for Data of Object.Framebuffers loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Data.Index);
            pragma Assert (Pass.Side_Effect or else Pass.References > 0);

            procedure Execute_Pass (Framebuffer : in out Rendering.Framebuffers.Framebuffer) is
            begin
               Framebuffer.Use_Framebuffer;

               GL.Buffers.Set_Depth_Mask (True);
               GL.Buffers.Set_Stencil_Mask (2#1111_1111#);
               if not Data.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Data.Clear_Buffers);
               end if;
               Framebuffer.Clear (Data.Clear_Mask);

               if not Data.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Data.Render_Buffers);
               end if;

               GL.Buffers.Set_Depth_Mask (Data.Depth_Writes);
               GL.Buffers.Set_Stencil_Mask (if Data.Stencil_Writes then 2#1111_1111# else 0);

               GL.Toggles.Set (GL.Toggles.Depth_Test, Pass.Has_Depth);
               GL.Toggles.Set (GL.Toggles.Stencil_Test, Pass.Has_Stencil);
               --  Note: unconditional writing (write + no test) would require
               --  enable testing + GL.Buffers.Set_Depth_Function (GL.Types.Always),
               --  but is not supported because otherwise a user would need to call
               --  Add_Input_Output instead of just Add_Output for a depth resource

               --  Apply GL state updates
               Context.Update_State (Pass.State);

               --  Bind textures and images
               for Resource of Object.Input_Resources (Pass) loop
                  case Resource.Mode is
                     when Texture_Read =>
                        Textures.Bind (Get_Texture (Resource.Data),
                          Textures.Texture, Natural (Resource.Binding));
                     when Image_Load =>
                        Textures.Bind (Get_Texture (Resource.Data),
                          Textures.Image, Natural (Resource.Binding));
                     when Not_Used | Framebuffer_Attachment =>
                        null;
                  end case;
               end loop;

               for Resource of Object.Output_Resources (Pass) loop
                  --  Resource has already been attached in procedure Initialize
                  --  if mode is Framebuffer_Attachment
                  if Resource.Mode = Image_Store then
                     Textures.Bind (Get_Texture (Resource.Data), Textures.Image,
                       Natural (Resource.Binding));
                  end if;
               end loop;

               if Execute /= null then
                  Execute (Pass);
               end if;

               pragma Assert (Framebuffer.Default = Pass.Present);

               if Pass.Present then
                  case Object.Present_Mode is
                     when Use_Default =>
                        --  User-defined program will use default framebuffer to render to screen
                        null;
                     when Blit_To_Default =>
                        declare
                           procedure Resolve_From_Pass
                             (Other_Framebuffer : Rendering.Framebuffers.Framebuffer) is
                           begin
                              Other_Framebuffer.Resolve_To (Framebuffer);
                           end Resolve_From_Pass;
                        begin
                           --  Blit input texture to screen
                           Object.Framebuffers (Object.Last_Pass_Index).Framebuffer.Query_Element
                             (Resolve_From_Pass'Access);
                        end;
                     when Render_To_Default =>
                        --  Render input texture to screen
                        Object.Present_Program.Use_Program;

                        GL.Buffers.Set_Depth_Function (GL.Types.Always);
                        Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
                        GL.Buffers.Set_Depth_Function (GL.Types.Greater);
                  end case;
               end if;

               --  Invalidate attachments that are transcient
               --  (not read by a subsequent render pass)
               Framebuffer.Invalidate (Data.Invalidate_Mask);
               --  TODO Use Data.Invalidate_Points for the color attachment points
            end Execute_Pass;
         begin
            Data.Framebuffer.Update_Element (Execute_Pass'Access);
         end;
      end loop;
   end Render;

   ----------------------------------------------------------------------

   procedure Write_Graph
     (Object   : in out Graph;
      Location : Resources.Locations.Writable_Location_Ptr;
      Path     : String)
   is
      package SU renames Ada.Strings.Unbounded;
      package SF renames Ada.Strings.Fixed;

      function Trim (Value : String) return String is (SF.Trim (Value, Ada.Strings.Both));

      function Image (Value : String)  return String is ('"' & Value & '"');
      function Image (Value : Integer) return String is (Trim (Value'Image));
      function Image (Value : Boolean) return String is (if Value then "true" else "false");

      Result : SU.Unbounded_String;
      First  : Boolean;

      procedure Append (Key, Value : String; Comma : Boolean := False) is
      begin
         SU.Append (Result, '"' & Key & '"' & ':' & Value);
         if Comma then
            SU.Append (Result, ',');
         end if;
      end Append;

      procedure Append_Comma is
      begin
         if not First then
            SU.Append (Result, ',');
         end if;
         First := False;
      end Append_Comma;
   begin
      SU.Append (Result, "{");

      --  Vertices (render passes and resources)
      First := True;
      Append ("passes", "[");
      for Pass of Object.Graph.Passes loop
         Append_Comma;
         SU.Append (Result, '{');
         Append ("name", Image (+Pass.Name), True);
         Append ("readCount", Image (Pass.Read_Count), True);
         Append ("writeCount", Image (Pass.Write_Count), True);
         Append ("sideEffect", Image (Pass.Side_Effect), True);
         Append ("references", Image (Pass.References));
         SU.Append (Result, '}');
      end loop;
      SU.Append (Result, "],");

      First := True;
      Append ("resources", "[");
      for Resource of Object.Graph.Resources loop
         Append_Comma;
         SU.Append (Result, '{');
         Append ("name", Image (+Resource.Description.Name), True);
         Append ("kind", Image (Resource.Description.Kind'Image), True);
         Append ("format", Image (Resource.Description.Format'Image), True);
         Append ("version", Image (Resource.Description.Version.Version), True);
         Append ("implicit", Image (Resource.Implicit), True);
         Append ("readMode", Image (Resource.Input_Mode'Image), True);
         Append ("writeMode", Image (Resource.Output_Mode'Image), True);
         Append ("readCount", Image (Resource.Read_Count), True);
         Append ("references", Image (Resource.References));
         SU.Append (Result, '}');
      end loop;
      SU.Append (Result, "],");

      --  Edges (reads and writes)
      First := True;
      Append ("reads", "[");
      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
         begin
            --  Passes reading from resources
            for Resource_Index in Pass.Read_Offset .. Pass.Read_Offset + Pass.Read_Count - 1 loop
               declare
                  Handle : Handle_Type renames Object.Graph.Read_Handles (Resource_Index);
               begin
                  Append_Comma;
                  SU.Append (Result, '{');
                  Append ("source", Image (Positive (Handle) - 1), True);
                  Append ("target", Image (Index  - 1));
                  SU.Append (Result, '}');
               end;
            end loop;
         end;
      end loop;
      SU.Append (Result, "],");

      First := True;
      Append ("writes", "[");
      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
         begin
            --  Passes writing to resources
            for Resource_Index in
              Pass.Write_Offset .. Pass.Write_Offset + Pass.Write_Count - 1
            loop
               declare
                  Handle : Handle_Type renames Object.Graph.Write_Handles (Resource_Index);
               begin
                  Append_Comma;
                  SU.Append (Result, '{');
                  Append ("source", Image (Index  - 1), True);
                  Append ("target", Image (Positive (Handle) - 1));
                  SU.Append (Result, '}');
               end;
            end loop;
         end;
      end loop;
      SU.Append (Result, "]");

      SU.Append (Result, "}");

      declare
         subtype JSON_Byte_Array is Resources.Byte_Array
           (1 .. Ada.Streams.Stream_Element_Offset (SU.Length (Result)));

         function Convert is new Ada.Unchecked_Conversion
           (Source => String, Target => JSON_Byte_Array);
      begin
         Location.Write_Data (Path, Convert (SU.To_String (Result)));
      end;
   end Write_Graph;

end Orka.Frame_Graphs;
