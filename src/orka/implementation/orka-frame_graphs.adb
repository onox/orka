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

with GL.Pixels.Extensions;
with GL.Types;

package body Orka.Frame_Graphs is

   package PE renames GL.Pixels.Extensions;

   type Attachment_Format is (Depth_Stencil, Depth, Stencil, Color);

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
     (Format : GL.Pixels.Internal_Format) return Attachment_Format is
   begin
      if PE.Depth_Stencil_Format (Format) then
         return Depth_Stencil;
      elsif PE.Depth_Format (Format) then
         return Depth;
      elsif PE.Stencil_Format (Format) then
         return Stencil;
      else
         return Color;
      end if;
   end Get_Attachment_Format;

   procedure Execute_Present (Pass : Render_Pass_Data) is null;

   use type GL.Objects.Textures.Texture;

   package Texture_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => GL.Objects.Textures.Texture,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Textures : Texture_Maps.Map;

   function Get_Texture (Subject : Resource) return GL.Objects.Textures.Texture is
   begin
      return Textures.Element (+Subject.Name);
   exception
      when Constraint_Error =>
         declare
            Result : GL.Objects.Textures.Texture (Subject.Kind);
         begin
            Result.Allocate_Storage
              (Levels  => GL.Types.Size (Subject.Levels),
               Samples => GL.Types.Size (Subject.Samples),
               Format  => Subject.Format,
               Width   => GL.Types.Size (Subject.Extent.Width),
               Height  => GL.Types.Size (Subject.Extent.Height),
               Depth   => GL.Types.Size (Subject.Extent.Depth));
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
      Handle   : out Handle_Type;
      Implicit : Boolean);

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      Execute : not null Execute_Callback;
      Side_Effect, Present : Boolean) return Render_Pass'Class is
   begin
      Object.Passes.Append
        ((Name        => +Name,
          Execute     => Execute,
          Side_Effect => Side_Effect,
          others      => <>));
      return Render_Pass'
        (Frame_Graph => Object'Access,
         Index       => Object.Passes.Length);
   end Add_Pass;

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      Execute : not null Execute_Callback;
      Side_Effect : Boolean := False) return Render_Pass'Class
   is (Object.Add_Pass (Name, Execute, Side_Effect => Side_Effect, Present => False));

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
           ("Present", Execute_Present'Access, Side_Effect => True, Present => True);
         Resource : Resource_Data renames Object.Resources (Handle);
      begin
         --  The present pass does not actually read the resource: the previous
         --  render pass will use the default framebuffer to write to the
         --  resource or the present pass will blit or render the resource to
         --  the default framebuffer
         Render_Pass (Pass).Add_Input (Subject, Resource.Input_Mode, Handle, Implicit => False);

         Object.Present_Pass := Pass.Index;
      end;
   end Add_Present;

   -----------------------------------------------------------------------------

   procedure Add_Output
     (Object   : Render_Pass;
      Subject  : Resource;
      Write    : Write_Mode;
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
            Object.Add_Input (Prev_Subject, Framebuffer_Attachment, Handle, Implicit => False);
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

         Resource.Render_Pass := Object.Index;
         Resource.Output_Mode := Write;
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
      Write   : Write_Mode)
   is
      Handle : Handle_Type;
      pragma Unreferenced (Handle);
   begin
      Object.Add_Output (Subject, Write, Handle, Implicit => True);
   end Add_Output;

   procedure Add_Input
     (Object   : Render_Pass;
      Subject  : Resource;
      Read     : Read_Mode;
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
            Object.Add_Output (Next_Subject, Framebuffer_Attachment, Handle, Implicit => False);
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

         Resource.Read_Count := Resource.Read_Count + 1;
         Resource.Input_Mode := Read;
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
      Read    : Read_Mode)
   is
      Handle : Handle_Type;
      pragma Unreferenced (Handle);
   begin
      Object.Add_Input (Subject, Read, Handle, Implicit => True);
   end Add_Input;

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Write   : Write_Mode) return Resource
   is
      Graph  : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
      Handle : Handle_Type;

      Next_Subject : Resource := Subject;
   begin
      Object.Add_Input (Subject, Read, Handle, Implicit => False);
      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         Resource.Modified := True;
      end;

      Next_Subject.Version.Version := Subject.Version.Version + 1;
      Object.Add_Output (Next_Subject, Write, Handle, Implicit => False);
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
                        Resource_Handle : constant Handle_Type := Result.Graph.Read_Handles (Index);
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

   procedure Initialize
     (Object  : in out Graph;
      Default : Rendering.Framebuffers.Framebuffer)
   is
      subtype Selector_Type is GL.Buffers.Explicit_Color_Buffer_Selector;
      subtype Buffer_Type   is GL.Buffers.Draw_Buffer_Index;
      subtype Point_Type    is Rendering.Framebuffers.Color_Attachment_Point;

      use type GL.Low_Level.Enums.Texture_Kind;

      type Present_Mode_Type is (Use_Default, Blit_To_Default, Render_To_Default);

      Present_Pass : Render_Pass_Data renames Object.Graph.Passes (Object.Graph.Present_Pass);
      pragma Assert (Present_Pass.Read_Count = 1);

      Default_Extent : constant Extent_3D := (Natural (Default.Width), Natural (Default.Height), 1);

      Last_Pass_Index : Positive;
      Present_Mode    : Present_Mode_Type;
   begin
      declare
         --  Look up the resource that is going to be presented to the screen
         Resource_Handle : constant Handle_Type
           := Object.Graph.Read_Handles (Present_Pass.Read_Offset);
         Resource : Resource_Data renames Object.Graph.Resources (Resource_Handle);

         --  Look up the last render pass *before* the present pass
         Last_Render_Pass : Render_Pass_Data renames Object.Graph.Passes (Resource.Render_Pass);

         Attachments : Natural := 0;
         Current_Point, Color_Attachment : Point_Type := Point_Type'First;
      begin
         Last_Pass_Index := Resource.Render_Pass;

         --  Mode 1: Previous render pass has one FB color attachment (this resource)
         --    Action: Previous render pass can use the default framebuffer
         --
         --  Mode 2: Previous render pass has multiple FB color attachments
         --          or resource is a depth and/or stencil attachment
         --    Action: Blit the resource to the default framebuffer
         --
         --  Mode 3: Resource is not written as a FB color attachment
         --    Action: Bind the resource as a texture and render to the
         --            default framebuffer
         if Resource.Output_Mode /= Framebuffer_Attachment then
            --  Mode 3
            --  TODO Bind resource as texture and render to default framebuffer
            Present_Mode := Render_To_Default;
            raise Program_Error with "Not implemented mode 3 yet";
         else
            for Last_Resource of Object.Output_Resources (Last_Render_Pass) loop
               if Last_Resource.Mode = Framebuffer_Attachment and then
                  Get_Attachment_Format (Last_Resource.Data.Format) = Color
               then
                  Attachments := Attachments + 1;

                  --  TODO Handle depth/stencil resources for mode 2
                  --  TODO Getting the attachment point is only needed for mode 2
                  if Last_Resource.Data = Resource.Description then
                     Color_Attachment := Current_Point;
                  end if;
                  Current_Point := Point_Type'Succ (Current_Point);
               end if;
            end loop;

            if Attachments = 1 and Get_Attachment_Format (Resource.Description.Format) = Color
              and Resource.Description.Kind = GL.Low_Level.Enums.Texture_2D
              and Resource.Description.Extent = Default_Extent
              and Resource.Description.Samples = Natural (Default.Samples)
            then
               --  Mode 1: Use Default as the framebuffer of Last_Render_Pass
               Present_Mode := Use_Default;
            else
               --  Mode 2
               --  TODO Last_Render_Pass.Framebuffer.Set_Read_Buffer (Color_Attachment);
               --  TODO Last_Render_Pass.Framebuffer.Resolve_To (Default);
               Present_Mode := Blit_To_Default;
               raise Program_Error with "Not implemented mode 2 yet";
            end if;
         end if;
      end;

      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
            Width, Height : Natural := Natural'First;

            Clear_Mask      : GL.Buffers.Buffer_Bits := (others => False);
            Invalidate_Mask : GL.Buffers.Buffer_Bits := (others => False);

            --  If Clear_Buffers = Render_Buffers then we only need to call
            --  Set_Draw_Buffers once after creating the framebuffer, otherwise
            --  buffers need to be set before clearing and rendering
            Clear_Buffers  : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
            Render_Buffers : GL.Buffers.Color_Buffer_List (0 .. 7) := (others => GL.Buffers.None);
            Buffers_Equal  : Boolean := False;
            --  TODO Use Draw_Buffer_Index as index type and drop some in Set_Draw_Buffers

            Invalidate_Points : Rendering.Framebuffers.Use_Point_Array := (others => False);

            --  For draw buffers
            Current_Selector : Selector_Type := Selector_Type'First;
            Current_Buffer   : Buffer_Type   := Buffer_Type'First;

            --  For invalidating color attachments
            Current_Point    : Point_Type    := Point_Type'First;

            Depth_Writes, Stencil_Writes : Boolean := True;

            Clear_Buffer, Invalidate_Buffer : Boolean;
         begin
            if Pass.Side_Effect or Pass.References > 0 then
               --  Clear input resources read as framebuffer attachments
               --  which are new (not written by a previous render pass)
               for Resource of Object.Input_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Clear_Buffer := not Resource.Written;

                     if PE.Depth_Stencil_Format (Resource.Data.Format) then
                        Clear_Mask.Depth   := Clear_Buffer;
                        Clear_Mask.Stencil := Clear_Buffer;
                     elsif PE.Depth_Format (Resource.Data.Format) then
                        Clear_Mask.Depth   := Clear_Buffer;
                     elsif PE.Stencil_Format (Resource.Data.Format) then
                        Clear_Mask.Stencil := Clear_Buffer;
                     else
                        --  Clear the color buffers, but only those that
                        --  do not have a render pass that writes to it
                        if Clear_Buffer then
                           Clear_Buffers (Current_Buffer) := Current_Selector;
                           Clear_Mask.Color := True;
                        end if;

                        Current_Selector := Selector_Type'Succ (Current_Selector);
                        Current_Buffer   := Buffer_Type'Succ (Current_Buffer);
                     end if;
                  end if;
               end loop;

               Current_Selector := Selector_Type'First;
               Current_Buffer   := Buffer_Type'First;

               --  Invalidate output attachments that are transcient
               --  (not read by a subsequent render pass)
               for Resource of Object.Output_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Invalidate_Buffer := not Resource.Read;

                     if PE.Depth_Stencil_Format (Resource.Data.Format) then
                        Invalidate_Mask.Depth   := Invalidate_Buffer;
                        Invalidate_Mask.Stencil := Invalidate_Buffer;

                        Depth_Writes   := not Resource.Implicit;
                        Stencil_Writes := not Resource.Implicit;
                     elsif PE.Depth_Format (Resource.Data.Format) then
                        Invalidate_Mask.Depth   := Invalidate_Buffer;

                        Depth_Writes   := not Resource.Implicit;
                     elsif PE.Stencil_Format (Resource.Data.Format) then
                        Invalidate_Mask.Stencil := Invalidate_Buffer;

                        Stencil_Writes := not Resource.Implicit;
                     else
                        --  Invalidate the color buffers, but only those that
                        --  are not read by a subsequent render pass
                        if Invalidate_Buffer then
                           Invalidate_Points (Current_Point) := True;
                           Invalidate_Mask.Color := True;
                        end if;

                        --  Even if the resource is not read, the buffer is
                        --  still used as a draw buffer because the shader
                        --  will probably render to it
                        Render_Buffers (Current_Buffer) := Current_Selector;

                        Current_Selector := Selector_Type'Succ (Current_Selector);
                        Current_Buffer   := Buffer_Type'Succ (Current_Buffer);

                        Current_Point := Point_Type'Succ (Current_Point);
                     end if;
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

               Object.Framebuffers.Append
                 ((Index       => Index,
                   Framebuffer => Framebuffer_Holders.To_Holder
                     (if Present_Mode = Use_Default and Last_Pass_Index = Index then
                        Default
                      else
                        Rendering.Framebuffers.Create_Framebuffer
                          (Width  => GL.Types.Size (Width),
                           Height => GL.Types.Size (Height))),

                   Clear_Mask        => Clear_Mask,
                   Invalidate_Mask   => Invalidate_Mask,

                   Clear_Buffers     => Clear_Buffers,
                   Render_Buffers    => Render_Buffers,
                   Buffers_Equal     => Buffers_Equal,

                   Invalidate_Points => Invalidate_Points,

                   Depth_Writes      => Depth_Writes,
                   Stencil_Writes    => Stencil_Writes));

               declare
                  procedure Set_Buffers
                    (Framebuffer : in out Rendering.Framebuffers.Framebuffer)
                  is
                     use type GL.Buffers.Color_Buffer_Selector;
                     use type GL.Types.UInt;
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

                        if Present_Mode = Use_Default then
                           pragma Assert (Render_Buffers
                             (Render_Buffers'First) = GL.Buffers.Color_Attachment0);
                           pragma Assert
                             (for all Index in Render_Buffers'First + 1 .. Render_Buffers'Last =>
                                Render_Buffers (Index) = GL.Buffers.None);
                           --  Not calling Set_Draw_Buffers because the default
                           --  framebuffer already has an initial draw buffer
                           --  (GL.Buffers.Back_Left for double-buffered context)
                        end if;
                     else
                        if Buffers_Equal then
                           Framebuffer.Set_Draw_Buffers (Render_Buffers);
                        end if;

                        Current_Point := Point_Type'First;

                        for Resource of Object.Input_Resources (Pass) loop
                           if Resource.Mode = Framebuffer_Attachment then
                              --  TODO Use Resource.Data.Extent.Depth if resource is layered
                              if Get_Attachment_Format (Resource.Data.Format) /= Color then
                                 Framebuffer.Attach (Get_Texture (Resource.Data));
                              else
                                 Framebuffer.Attach
                                   (Current_Point, Get_Texture (Resource.Data));
                                 Current_Point := Point_Type'Succ (Current_Point);
                              end if;
                           end if;
                        end loop;
                     end if;
                  end Set_Buffers;
               begin
                  Object.Framebuffers
                    (Object.Framebuffers.Length).Framebuffer.Update_Element
                       (Set_Buffers'Access);
               end;
            end if;
         end;
      end loop;
   end Initialize;

   procedure Render (Object : in out Graph) is
   begin
      for Data of Object.Framebuffers loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Data.Index);
            pragma Assert (Pass.Side_Effect or else Pass.References > 0);

            procedure Execute_Pass (Framebuffer : in out Rendering.Framebuffers.Framebuffer) is
            begin
               Framebuffer.Use_Framebuffer;

               --  TODO Only change pipeline state if different w.r.t. previous pass
               GL.Buffers.Set_Depth_Mask (True);
               GL.Buffers.Set_Stencil_Mask (2#1111_1111#);
               if not Data.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Data.Clear_Buffers);
               end if;
               Framebuffer.Clear (Data.Clear_Mask);

               --  TODO Bind textures and images (or in procedure Cull?)
               --  (for Resource of Object.Input_Resources (Pass))

               if not Data.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Data.Render_Buffers);
               end if;

               --  TODO Only change pipeline state if different w.r.t. previous pass
               GL.Buffers.Set_Depth_Mask (Data.Depth_Writes);
               GL.Buffers.Set_Stencil_Mask (if Data.Stencil_Writes then 2#1111_1111# else 0);

               Pass.Execute (Pass);

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
                                  Read     => Data.References > 0,
                                  Implicit => Data.Implicit);
            end;
         end loop;
      end return;
   end Output_Resources;

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
            for Resource_Index in Pass.Write_Offset .. Pass.Write_Offset + Pass.Write_Count - 1 loop
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
