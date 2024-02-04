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

with GL.Objects.Textures;
with GL.Toggles;
with GL.Types;

with Orka.Logging.Default;
with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;
with Orka.Strings;
with Orka.Types;

package body Orka.Frame_Graphs is

   --  Present pass is always the default framebuffer. Make sure
   --  the color buffer of the default framebuffer is cleared when
   --  we manually need to blit or render some texture to the screen
   Render_Pass_Framebuffer_State : constant Framebuffer_State :=
     (Buffers_Equal  => True,
      Depth_Writes   => True,
      Stencil_Writes => True,
      Clear_Mask     => (Color => True, others => False),
      others         => <>);

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Default.Generic_Log (Renderer);

   function Trim_Image (Value : Integer) return String is
     (Orka.Strings.Trim (Integer'Image (Value)));

   function Trim_Image (Value : Size) return String is
     (Orka.Strings.Trim (Value'Image));

   function Trim_Image (Value : Render_Pass_Index) return String is
     (Trim_Image (Positive (Value)));

   function Trim_Image (Value : Size_3D) return String is
     (Trim_Image (Value (X)) & Orka.Strings.Unicode (" × ") &
      Trim_Image (Value (Y)) & Orka.Strings.Unicode (" × ") &
      Trim_Image (Value (Z)));

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
      Log (Debug, "      size:     " & Trim_Image (Value.Size));
      Log (Debug, "      samples:  " & Trim_Image (Value.Samples));
      Log (Debug, "      version:  " & Trim_Image (Natural (Value.Version)));
   end Log_Resource;

   function Name (Object : Resource_Data) return String is (+Object.Description.Name);

   function "+" (Value : Name_Strings.Bounded_String) return String is
     (Name_Strings.To_String (Value));

   function "+" (Value : String) return Name_Strings.Bounded_String is
     (Name_Strings.To_Bounded_String (Value));

   function Name (Object : Render_Pass) return String is
     (+Object.Frame_Graph.Passes (Object.Index).Name);

   function Name (Pass : Render_Pass_Data) return String is (+Pass.Name);

   -----------------------------------------------------------------------------

   procedure Find_Resource
     (Object  : Frame_Graph;
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
     (Object  : in out Frame_Graph;
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
               Width   => Subject.Size (X),
               Height  => Subject.Size (Y),
               Depth   => Subject.Size (Z));
            Textures.Insert (+Subject.Name, Result);
            return Result;
         end;
   end Get_Texture;

   -----------------------------------------------------------------------------

   procedure Add_Input
     (Object   : Render_Pass;
      Subject  : Resource;
      Mode     : Read_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean);

   function Add_Pass
     (Object   : in out Frame_Graph;
      Name     : String;
      State    : Rendering.States.State;
      Program  : Rendering.Programs.Program;
      Callback : not null Program_Callback;
      Side_Effect : Boolean := False) return Render_Pass'Class is
   begin
      Object.Passes.Append
        ((Name        => +Name,
          Side_Effect => Side_Effect,
          State       => State,
          Program     => Program,
          Callback    => Callback,
          others      => <>));
      return Render_Pass'
        (Frame_Graph => Object'Access,
         Index       => Object.Passes.Length);
   end Add_Pass;

   -----------------------------------------------------------------------------

   procedure Add_Output
     (Object   : Render_Pass;
      Subject  : Resource;
      Write    : Write_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean)
   is
      Graph : Orka.Frame_Graphs.Frame_Graph renames Object.Frame_Graph.all;
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
            Prev_Subject.Version := Subject.Version - 1;
            Object.Add_Input
              (Prev_Subject, Framebuffer_Attachment, Binding, Handle, Implicit => False);
            Graph.Resources (Handle).Implicit := True;
         end;
      end if;

      Add_Resource (Graph, Subject, Handle);

      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         if Resource.Render_Pass /= No_Render_Pass then
            raise Constraint_Error with "Resource '" & Name (Resource) & "'" &
              " already written by pass '" & Name (Graph.Passes (Resource.Render_Pass)) & "'";
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
      Mode    : Write_Mode;
      Binding : Binding_Point)
   is
      Handle : Handle_Type;
   begin
      Object.Add_Output (Subject, Mode, Binding, Handle, Implicit => True);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Output_Mode = Mode);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Output_Binding = Binding);
   end Add_Output;

   procedure Add_Input
     (Object   : Render_Pass;
      Subject  : Resource;
      Mode     : Read_Mode;
      Binding  : Binding_Point;
      Handle   : out Handle_Type;
      Implicit : Boolean)
   is
      Graph : Orka.Frame_Graphs.Frame_Graph renames Object.Frame_Graph.all;
      Pass  : Render_Pass_Data renames Graph.Passes (Object.Index);

      Attachment : constant Attachment_Format := Get_Attachment_Format (Subject.Format);
   begin
      if Pass.Read_Count > 0 and then
        Pass.Read_Offset + Pass.Read_Count /= Graph.Read_Handles.Length + 1
      then
         raise Program_Error with "Cannot interleave Add_Input calls for different passes";
      end if;

      if Implicit and Mode = Framebuffer_Attachment then
         if Attachment = Color then
            raise Program_Error with "Use Add_Output or Add_Input_Output for color resource";
         end if;

         Verify_Depth_Stencil (Pass, Attachment);

         declare
            Handle : Handle_Type;
            Next_Subject : Resource := Subject;
         begin
            Next_Subject.Version := Subject.Version + 1;
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
         if Resource.Input_Mode not in Not_Used | Mode then
            raise Constraint_Error with
              "Resource '" & Name (Resource) & "' must be read as " & Resource.Input_Mode'Image;
         end if;

         Resource.Read_Count    := Resource.Read_Count + 1;
         Resource.Input_Mode    := Mode;
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
      Mode    : Read_Mode;
      Binding : Binding_Point)
   is
      Handle : Handle_Type;
   begin
      Object.Add_Input (Subject, Mode, Binding, Handle, Implicit => True);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Input_Mode = Mode);
      pragma Assert (Object.Frame_Graph.Resources (Handle).Input_Binding = Binding);
   end Add_Input;

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Mode    : Read_Write_Mode;
      Binding : Binding_Point) return Resource
   is
      Graph  : Orka.Frame_Graphs.Frame_Graph renames Object.Frame_Graph.all;
      Handle : Handle_Type;

      Read : constant Read_Mode :=
        (case Mode is
           when Not_Used               => Not_Used,
           when Framebuffer_Attachment => Framebuffer_Attachment,
           when Image_Load_Store       => Image_Load);

      Write : constant Write_Mode :=
        (case Mode is
           when Not_Used               => Not_Used,
           when Framebuffer_Attachment => Framebuffer_Attachment,
           when Image_Load_Store       => Image_Store);

      Next_Subject : Resource := Subject;
   begin
      Object.Add_Input (Subject, Read, Binding, Handle, Implicit => False);
      declare
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         Resource.Modified := True;
      end;

      Next_Subject.Version := Subject.Version + 1;
      Object.Add_Output (Next_Subject, Write, Binding, Handle, Implicit => False);
      return Next_Subject;
   end Add_Input_Output;

   procedure Cull (Object : in out Renderable_Graph) is
      Stack : Handle_Vectors.Vector (Positive (Object.Graph.Resources.Length));
      Index : Handle_Type;
   begin
      --  Raise an error if there is a render pass that will be
      --  culled immediately. This simplifies the stack so that it
      --  only needs to contain indices of resources.
      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
            References : Natural renames Object.Render_Pass_References (Index);
         begin
            References := Pass.Write_Count;

            if not Pass.Side_Effect and References = 0 then
               raise Constraint_Error with
                 "Render pass '" & (+Pass.Name) & "' does not write to any resource";
            end if;
         end;
      end loop;

      for Index in 1 .. Object.Graph.Resources.Length loop
         declare
            Resource : Resource_Data renames Object.Graph.Resources (Index);
            References : Natural renames Object.Resource_References (Index);
         begin
            --  Increment read count for resource read by present pass
            References := Resource.Read_Count + (if Index = Object.Present_Resource then 1 else 0);

            if References = 0 then
               if Resource.Render_Pass = No_Render_Pass then
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
            Resource : Resource_Data renames Object.Graph.Resources (Index);
            Pass : Render_Pass_Data renames Object.Graph.Passes (Resource.Render_Pass);

            Render_Pass_References : Natural renames Object.Render_Pass_References (Resource.Render_Pass);

            --  Assert that the render pass does write to the resource
            Write_Offset : Positive renames Pass.Write_Offset;
            Write_Count  : Natural  renames Pass.Write_Count;
            pragma Assert
              (for some Offset in Write_Offset .. Write_Offset + Write_Count - 1 =>
                 Object.Graph.Write_Handles (Offset) = Index);
         begin
            Render_Pass_References := Render_Pass_References - 1;

            --  Update ref count of resources read by the render pass
            --  if the render pass got culled
            if not Pass.Side_Effect and Render_Pass_References = 0 then
               for Index in Pass.Read_Offset .. Pass.Read_Offset + Pass.Read_Count - 1 loop
                  declare
                     Resource_Handle : constant Handle_Type :=
                       Object.Graph.Read_Handles (Index);
                     Resource : Resource_Data renames
                       Object.Graph.Resources (Resource_Handle);

                     Resource_References : Natural renames Object.Resource_References (Resource_Handle);
                  begin
                     Resource_References := Resource_References - 1;

                     if Resource_References = 0 and Resource.Render_Pass /= No_Render_Pass then
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
   end Cull;

   function Input_Resources
     (Object : Renderable_Graph;
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
                                  Written  => Data.Render_Pass /= No_Render_Pass,
                                  Implicit => Data.Implicit);
            end;
         end loop;
      end return;
   end Input_Resources;

   function Output_Resources
     (Object : Renderable_Graph;
      Pass   : Render_Pass_Data) return Output_Resource_Array is
   begin
      return Result : Output_Resource_Array (1 .. Pass.Write_Count) do
         for Index in 1 .. Pass.Write_Count loop
            declare
               Handle : constant Handle_Type := Object.Graph.Write_Handles (Pass.Write_Offset + Index - 1);
               Data : Resource_Data renames Object.Graph.Resources (Handle);
               References : Natural renames Object.Resource_References (Handle);
            begin
               Result (Index) := (Mode     => Data.Output_Mode,
                                  Data     => Data.Description,
                                  Binding  => Data.Output_Binding,
                                  Read     => References > 0,
                                  Implicit => Data.Implicit);
            end;
         end loop;
      end return;
   end Output_Resources;

   procedure Determine_Present_Mode
     (Object           : in out Renderable_Graph;
      Location         : Resources.Locations.Location_Ptr;
      Default          : Rendering.Framebuffers.Framebuffer;
      Last_Render_Pass : Render_Pass_Data;
      Resource         : Resource_Data)
   is
      package Programs renames Orka.Rendering.Programs;

      Default_Size : constant Size_3D := (Default.Width, Default.Height, 1);

      Format : constant Attachment_Format :=
        Get_Attachment_Format (Resource.Description.Format);

      Attachments : Natural := 0;
      Has_Non_Color_Attachment : Boolean := False;
   begin
      --  Program of present pass is needed when rendering to default framebuffer (mode 3)
      Object.Present_Render_Pass :=
        (Name    => +"Present",
         Program =>
          Programs.Create_Program (Programs.Modules.Create_Module
            (Location,
             VS => "oversized-triangle.vert",
             FS => "frame-graph-present.frag")),
         Side_Effect => True,
         Read_Count  => 1,
         others      => <>);

      Object.Present_Render_Pass.Program.Uniform ("screenResolution").Set_Vector
        (Orka.Types.Singles.Vector4'
          (Orka.Float_32 (Default.Width), Orka.Float_32 (Default.Height), 0.0, 0.0));

      --  Mode 1: Previous render pass has one FB color attachment (this resource)
      --    Action: Previous render pass can use the default framebuffer
      --
      --  Mode 2: Previous render pass has multiple FB color attachments
      --          or other non-color FB attachments
      --    Action: Blit the resource to the default framebuffer by the present pass
      --
      --  Mode 3: Resource is not written as a FB color attachment
      --          or resource is a depth and/or stencil attachment
      --    Action: Bind the resource as a texture and render to the
      --            default framebuffer by the present pass
      if Resource.Output_Mode /= Framebuffer_Attachment or Format /= Color then
         --  Mode 3: Bind resource as texture and render to default framebuffer
         Object.Present_Mode := Render_To_Default;

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
           and Resource.Description.Kind = Texture_2D
           and Resource.Description.Size = Default_Size
           and Resource.Description.Samples = Natural (Default.Samples)
         then
            --  Mode 1: Use Default as the framebuffer of Last_Render_Pass
            Object.Present_Mode := Use_Default;

            Log (Debug, "Presenting " & Name (Resource) & " using default framebuffer");
         elsif Resource.Description.Kind in Texture_2D | Texture_2D_Multisample then
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

            if Resource.Description.Kind /= Texture_2D then
               Log (Warning, "  kind: " & Resource.Description.Kind'Image &
                 " (/= " & Texture_2D'Image & ")");
            end if;

            if Resource.Description.Size /= Default_Size then
               Log (Warning, "  scaling: from " &
                 Trim_Image (Resource.Description.Size) & " to " &
                 Trim_Image (Default_Size));
            end if;

            if Resource.Description.Samples /= Natural (Default.Samples) then
               Log (Warning, "  samples: " &
                 Trim_Image (Resource.Description.Samples) & " (/= " &
                 Trim_Image (Natural (Default.Samples)) & ")");
            end if;
         end if;
      end if;
   end Determine_Present_Mode;

   procedure Initialize
     (Object   : in out Renderable_Graph;
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

      --  Look up the last render pass *before* the present pass (e.g. the pass which writes to the presented resource)
      Present_Resource : Resource_Data renames Object.Graph.Resources (Object.Present_Resource);
      Last_Pass_Index : constant Render_Pass_Index := Present_Resource.Render_Pass;
   begin
      Object.Determine_Present_Mode (Location, Default, Object.Graph.Passes (Last_Pass_Index), Present_Resource);

      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
            References : Natural renames Object.Render_Pass_References (Index);

            Width, Height       : Size    := Size'First;
            Samples_Attachments : Natural := Natural'First;

            --  If Clear_Buffers = Render_Buffers then we only need to call
            --  Set_Draw_Buffers once after creating the framebuffer, otherwise
            --  buffers need to be set before clearing and rendering
            State : Framebuffer_State := (Buffers_Equal => False, Depth_Writes | Stencil_Writes => True, others => <>);

            Clear_Buffer, Invalidate_Buffer : Boolean;
         begin
            if Pass.Side_Effect or References > 0 then
               --  Clear input resources read as framebuffer attachments
               --  which are new (not written by a previous render pass)
               for Resource of Object.Input_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Clear_Buffer := not Resource.Written;

                     case Get_Attachment_Format (Resource.Data.Format) is
                        when Depth_Stencil =>
                           State.Clear_Mask.Depth   := Clear_Buffer;
                           State.Clear_Mask.Stencil := Clear_Buffer;
                        when Depth =>
                           State.Clear_Mask.Depth   := Clear_Buffer;
                        when Stencil =>
                           State.Clear_Mask.Stencil := Clear_Buffer;
                        when Color =>
                           --  Clear the color buffers, but only those that
                           --  do not have a render pass that writes to it
                           if Clear_Buffer then
                              State.Clear_Buffers (Buffer_Type (Resource.Binding)) :=
                                To_Color_Buffer (Resource.Binding);
                              State.Clear_Mask.Color := True;
                           end if;
                     end case;
                  end if;
               end loop;

               --  Invalidate output attachments that are transcient
               --  (not read by a subsequent render pass)
               for Resource of Object.Output_Resources (Pass) loop
                  if Resource.Mode = Framebuffer_Attachment then
                     Invalidate_Buffer := not Resource.Read;

                     Samples_Attachments :=
                       Natural'Max (Samples_Attachments, Resource.Data.Samples);

                     case Get_Attachment_Format (Resource.Data.Format) is
                        when Depth_Stencil =>
                           State.Invalidate_Mask.Depth   := Invalidate_Buffer;
                           State.Invalidate_Mask.Stencil := Invalidate_Buffer;

                           State.Depth_Writes   := not Resource.Implicit;
                           State.Stencil_Writes := not Resource.Implicit;
                        when Depth =>
                           State.Invalidate_Mask.Depth   := Invalidate_Buffer;

                           State.Depth_Writes   := not Resource.Implicit;
                        when Stencil =>
                           State.Invalidate_Mask.Stencil := Invalidate_Buffer;

                           State.Stencil_Writes := not Resource.Implicit;
                        when Color =>
                           --  Invalidate the color buffers, but only those that
                           --  are not read by a subsequent render pass
                           if Invalidate_Buffer then
                              State.Invalidate_Points (To_Attachment_Point (Resource.Binding)) := True;
                              State.Invalidate_Mask.Color := True;
                           end if;

                           --  Even if the resource is not read, the buffer is
                           --  still used as a draw buffer because the shader
                           --  will probably render to it
                           State.Render_Buffers (Buffer_Type (Resource.Binding)) :=
                             To_Color_Buffer (Resource.Binding);
                     end case;
                  end if;

                  --  Compute maximum width and height over all the output
                  --  resources (which may be attached to the framebuffer).
                  --  Ideally all attachments have the same size, but the
                  --  GL spec allows for them to be different. Furthermore,
                  --  a framebuffer may have zero attachments, so iterate over
                  --  all resources irrespective of their write mode.
                  Width  := Size'Max (Width, Resource.Data.Size (X));
                  Height := Size'Max (Height, Resource.Data.Size (Y));
               end loop;

               if Pass.Write_Count = 0 then
                  pragma Assert (Pass.Side_Effect);

                  --  Use width and height from default framebuffer
                  Width  := Default.Width;
                  Height := Default.Height;
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
                  State.Buffers_Equal := State.Clear_Buffers = State.Render_Buffers;
               end;

               Object.Framebuffers.Append
                 ((Index       => Index,
                   Framebuffer => Framebuffer_Holders.To_Holder
                     (if Object.Present_Mode = Use_Default and Last_Pass_Index = Index then
                        Default
                      else
                        Rendering.Framebuffers.Create_Framebuffer
                          (Width   => Width,
                           Height  => Height,
                           Samples => Size (Samples_Attachments))),
                   State             => State));

               if Object.Present_Mode = Use_Default and Last_Pass_Index = Index then
                  Object.Present_Pass := Index;
               elsif Object.Present_Mode = Blit_To_Default and Last_Pass_Index = Index then
                  Object.Last_FB_Index := Object.Framebuffers.Length;
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
                        pragma Assert (not State.Invalidate_Mask.Color);

                        if not State.Buffers_Equal then
                           pragma Assert
                             (for all Buffer of State.Clear_Buffers => Buffer = GL.Buffers.None);
                        end if;

                        if Object.Present_Mode = Use_Default then
                           pragma Assert (State.Render_Buffers
                             (State.Render_Buffers'First) = GL.Buffers.Color_Attachment0);
                           pragma Assert
                             (for all Index in State.Render_Buffers'First + 1 .. State.Render_Buffers'Last
                                => State.Render_Buffers (Index) = GL.Buffers.None);
                           --  Not calling Set_Draw_Buffers because the default
                           --  framebuffer already has an initial draw buffer
                           --  (GL.Buffers.Back_Left for double-buffered context)
                        end if;
                     else
                        if Object.Present_Mode = Blit_To_Default and Last_Pass_Index = Index then
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

                        if State.Buffers_Equal then
                           Framebuffer.Set_Draw_Buffers (State.Render_Buffers);
                        end if;

                        for Resource of Object.Input_Resources (Pass) loop
                           if Resource.Mode = Framebuffer_Attachment then
                              --  TODO Support attaching layer of resource
                              --       using value in 1 .. Resource.Data.Size (Z)
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
         end;
      end loop;
   end Initialize;

   function Get_References (Object : Renderable_Graph; Index : Render_Pass_Index) return Natural is
     (Object.Render_Pass_References (Index));
   --  Function expression to avoid a GNAT BUG box

   procedure Log_Graph (Object : in out Renderable_Graph; Default : Rendering.Framebuffers.Framebuffer) is
      procedure Log_Pass
        (Index            : Render_Pass_Index;
         Framebuffer      : Rendering.Framebuffers.Framebuffer;
         State            : Framebuffer_State;
         Pass             : Render_Pass_Data;
         Input_Resources  : Input_Resource_Array;
         Output_Resources : Output_Resource_Array;
         References       : Natural;
         Is_Present       : Boolean)
      is
         use type GL.Buffers.Explicit_Color_Buffer_Selector;
      begin
         Log (Debug, "Pass " & Name (Pass) & " (" & Trim_Image (Index) & ")");
         Log (Debug, "  count:");
         Log (Debug, "    read:  " & Pass.Read_Count'Image);
         Log (Debug, "    write: " & Pass.Write_Count'Image);
         Log (Debug, "  references:   " & References'Image);
         Log (Debug, "  side effects: " & Pass.Side_Effect'Image);
         Log (Debug, "  present:      " & (if Is_Present then Object.Present_Mode'Image else "-"));

         if Is_Present and Object.Present_Mode = Blit_To_Default then
            Log (Debug, "    from pass: " & Trim_Image (Object.Framebuffers (Object.Last_FB_Index).Index));
         end if;

         Log (Debug, "  framebuffer: " & Framebuffer.Image);
         Log (Debug, "    writes:");
         Log (Debug, "      depth:   " & State.Depth_Writes'Image);
         Log (Debug, "      stencil: " & State.Stencil_Writes'Image);
         Log (Debug, "    masks:");
         Log (Debug, "      clear:      " & Trim_Image (State.Clear_Mask));
         Log (Debug, "      invalidate: " & Trim_Image (State.Invalidate_Mask));

         if not Framebuffer.Default then
            Log (Debug, "    invalidate:");
            for I in State.Invalidate_Points'Range loop
               if State.Invalidate_Points (I) then
                  Log (Debug, "    - " & I'Image);
               end if;
            end loop;
            Log (Debug, "    buffers:");
            Log (Debug, "      clear:");
            for I in State.Clear_Buffers'Range loop
               if State.Clear_Buffers (I) /= GL.Buffers.None then
                  Log (Debug, "        - " & State.Clear_Buffers (I)'Image);
               end if;
            end loop;
            if not State.Buffers_Equal then
               Log (Debug, "      render:");
               for I in State.Render_Buffers'Range loop
                  if State.Clear_Buffers (I) /= GL.Buffers.None then
                     Log (Debug, "        - " & State.Render_Buffers (I)'Image);
                  end if;
               end loop;
            end if;
         end if;

         Log (Debug, "  inputs:");
         for Resource of Input_Resources loop
            Log_Resource (Resource.Data);
            Log (Debug, "      mode:     " & Resource.Mode'Image);
            Log (Debug, "      binding:  " & Trim_Image (Natural (Resource.Binding)));
            Log (Debug, "      implicit: " & Resource.Implicit'Image);
            Log (Debug, "      written:  " & Resource.Written'Image);
         end loop;

         Log (Debug, "  outputs:");
         for Resource of Output_Resources loop
            Log_Resource (Resource.Data);
            Log (Debug, "      mode:     " & Resource.Mode'Image);
            Log (Debug, "      binding:  " & Trim_Image (Natural (Resource.Binding)));
            Log (Debug, "      implicit: " & Resource.Implicit'Image);
            Log (Debug, "      read:     " & Resource.Read'Image);
         end loop;
      end Log_Pass;
   begin
      for Data of Object.Framebuffers loop
         declare
            Pass  : Render_Pass_Data  renames Object.Graph.Passes (Data.Index);
            References : Natural renames Get_References (Object, Data.Index);
            pragma Assert (Pass.Side_Effect or else References > 0);

            procedure Log_Pass (Framebuffer : Rendering.Framebuffers.Framebuffer) is
            begin
               Log_Pass
                 (Index            => Data.Index,
                  Framebuffer      => Framebuffer,
                  State            => Data.State,
                  Pass             => Pass,
                  Input_Resources  => Object.Input_Resources (Pass),
                  Output_Resources => Object.Output_Resources (Pass),
                  References       => References,
                  Is_Present       => Object.Present_Pass = Data.Index);
            end Log_Pass;
         begin
            Data.Framebuffer.Query_Element (Log_Pass'Access);
         end;
      end loop;

      if Object.Present_Mode /= Use_Default then
         declare
            Present_Resource : Resource_Data renames Object.Graph.Resources (Object.Present_Resource);
         begin
            Log_Pass
              (Index       => Object.Framebuffers.Length + 1,
               Framebuffer => Default,
               State       => Render_Pass_Framebuffer_State,
               Pass        => Object.Present_Render_Pass,
               Input_Resources =>
                 (1 =>
                   (Mode     => Texture_Read,
                    Data     => Present_Resource.Description,
                    Binding  => 0,
                    Written  => True,
                    Implicit => False)),
               Output_Resources => (1 .. 0 => <>),
               References  => 0,
               Is_Present  => True);
         end;
      end if;
   end Log_Graph;

   procedure Render
     (Object  : in out Renderable_Graph;
      Context : in out Contexts.Context'Class;
      Default : Rendering.Framebuffers.Framebuffer)
   is
      package Textures renames Orka.Rendering.Textures;

      procedure Execute_Pass
        (Framebuffer      : in out Rendering.Framebuffers.Framebuffer;
         State            : Framebuffer_State;
         Pass             : Render_Pass_Data;
         Input_Resources  : Input_Resource_Array;
         Output_Resources : Output_Resource_Array;
         Is_Present       : Boolean) is
      begin
         Framebuffer.Use_Framebuffer;

         GL.Buffers.Set_Depth_Mask (True);
         GL.Buffers.Set_Stencil_Mask (2#1111_1111#);

         if State.Buffers_Equal then
            Framebuffer.Clear (State.Clear_Mask);
         else
            Framebuffer.Set_Draw_Buffers (State.Clear_Buffers);
            Framebuffer.Clear (State.Clear_Mask);
            Framebuffer.Set_Draw_Buffers (State.Render_Buffers);
         end if;

         GL.Buffers.Set_Depth_Mask (State.Depth_Writes);
         GL.Buffers.Set_Stencil_Mask (if State.Stencil_Writes then 2#1111_1111# else 0);

         GL.Toggles.Set (GL.Toggles.Depth_Test, Pass.Has_Depth);
         GL.Toggles.Set (GL.Toggles.Stencil_Test, Pass.Has_Stencil);
         --  Note: unconditional writing (write + no test) would require
         --  enable testing + GL.Buffers.Set_Depth_Function (GL.Types.Always),
         --  but is not supported because otherwise a user would need to call
         --  Add_Input_Output instead of just Add_Output for a depth resource

         --  Apply GL state updates
         Context.Update_State (Pass.State);
         --  FIXME Update Pass.State.Stenciling[Face].Write_Mask using Depth.Stencil_Writes?

         --  Bind textures and images
         for Resource of Input_Resources loop
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

         for Resource of Output_Resources loop
            --  Resource has already been attached in procedure Initialize
            --  if mode is Framebuffer_Attachment
            if Resource.Mode = Image_Store then
               Textures.Bind (Get_Texture (Resource.Data), Textures.Image,
                 Natural (Resource.Binding));
            end if;
         end loop;

         pragma Assert (Framebuffer.Default = Is_Present);

         if Is_Present then
            case Object.Present_Mode is
               when Use_Default =>
                  --  User-defined program will use default framebuffer to render to screen
                  Pass.Program.Use_Program;
                  Pass.Callback (Pass.Program);
               when Blit_To_Default =>
                  declare
                     procedure Resolve_From_Pass
                       (Other_Framebuffer : Rendering.Framebuffers.Framebuffer) is
                     begin
                        Other_Framebuffer.Resolve_To (Framebuffer);
                     end Resolve_From_Pass;
                  begin
                     --  Blit input texture to screen
                     Object.Framebuffers (Object.Last_FB_Index).Framebuffer.Query_Element
                       (Resolve_From_Pass'Access);
                  end;
               when Render_To_Default =>
                  --  Render input texture to screen
                  Pass.Program.Use_Program;

                  GL.Buffers.Set_Depth_Function (GL.Types.Always);
                  Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);
                  GL.Buffers.Set_Depth_Function (Pass.State.Depth_Func);
            end case;
         else
            Pass.Program.Use_Program;
            Pass.Callback (Pass.Program);
         end if;

         --  Invalidate attachments that are transcient
         --  (not read by a subsequent render pass)
         Framebuffer.Invalidate (State.Invalidate_Mask);
         --  TODO Use State.Invalidate_Points for the color attachment points
      end Execute_Pass;
   begin
      for Data of Object.Framebuffers loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Data.Index);
            References : Natural renames Get_References (Object, Data.Index);
            pragma Assert (Pass.Side_Effect or else References > 0);

            procedure Execute_Pass (Framebuffer : in out Rendering.Framebuffers.Framebuffer) is
            begin
               Execute_Pass
                 (Framebuffer      => Framebuffer,
                  State            => Data.State,
                  Pass             => Pass,
                  Input_Resources  => Object.Input_Resources (Pass),
                  Output_Resources => Object.Output_Resources (Pass),
                  Is_Present       => Object.Present_Mode = Use_Default and Object.Present_Pass = Data.Index);
            end Execute_Pass;
         begin
            Data.Framebuffer.Update_Element (Execute_Pass'Access);
         end;
      end loop;

      if Object.Present_Mode /= Use_Default then
         declare
            Present_Resource : Resource_Data renames Object.Graph.Resources (Object.Present_Resource);
            Attachment : constant Attachment_Format := Get_Attachment_Format (Present_Resource.Description.Format);

            Default_Framebuffer : Rendering.Framebuffers.Framebuffer := Default;
         begin
            Set_Depth_Stencil (Object.Present_Render_Pass, Attachment);

            Execute_Pass
              (Framebuffer => Default_Framebuffer,
               State       => Render_Pass_Framebuffer_State,
               Pass        => Object.Present_Render_Pass,
               Input_Resources  =>
                 (1 =>
                   (Mode     => Texture_Read,
                    Data     => Present_Resource.Description,
                    Binding  => 0,
                    Written  => True,
                    Implicit => False)),
               Output_Resources => (1 .. 0 => <>),
               Is_Present       => True);
         end;
      end if;
   end Render;

   procedure Render
     (Object  : in out Renderable_Graph;
      Context : in out Contexts.Context'Class;
      Location    : Resources.Locations.Location_Ptr;
      Framebuffer : Rendering.Framebuffers.Framebuffer;
      Present : Resource)
   is
      Index : Handle_Type;
      Found : Boolean;
   begin
      Object.Graph.Find_Resource (Present, Index, Found);

      if not Found then
         raise Constraint_Error with "Presented resource not found in graph";
      end if;

      if Index /= Object.Present_Resource then
         if Object.Graph.Resources (Index).Render_Pass = No_Render_Pass then
            raise Constraint_Error with "Presented resource not written by a pass";
         end if;

         Object.Present_Resource := Index;
         Object.Cull;

         Object.Framebuffers.Clear;
         Object.Initialize (Location, Framebuffer);
      end if;

      Object.Render (Context, Framebuffer);
   end Render;

   ----------------------------------------------------------------------

   procedure Write_Graph
     (Object   : in out Renderable_Graph;
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
      for Index in 1 .. Object.Graph.Passes.Length loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Index);
            References : Natural renames Object.Render_Pass_References (Index);
         begin
            Append_Comma;
            SU.Append (Result, '{');
            Append ("name", Image (+Pass.Name), True);
            Append ("readCount", Image (Pass.Read_Count), True);
            Append ("writeCount", Image (Pass.Write_Count), True);
            Append ("sideEffect", Image (Pass.Side_Effect), True);
            Append ("references", Image (References));
            SU.Append (Result, '}');
         end;
      end loop;
      SU.Append (Result, "],");

      First := True;
      Append ("resources", "[");
      for Index in 1 .. Object.Graph.Resources.Length loop
         declare
            Resource : Resource_Data renames Object.Graph.Resources (Index);
            References : Natural renames Object.Resource_References (Index);
         begin
            Append_Comma;
            SU.Append (Result, '{');
            Append ("name", Image (+Resource.Description.Name), True);
            Append ("kind", Image (Resource.Description.Kind'Image), True);
            Append ("format", Image (Resource.Description.Format'Image), True);
            Append ("version", Image (Natural (Resource.Description.Version)), True);
            Append ("implicit", Image (Resource.Implicit), True);
            Append ("readMode", Image (Resource.Input_Mode'Image), True);
            Append ("writeMode", Image (Resource.Output_Mode'Image), True);
            Append ("readCount", Image (Resource.Read_Count), True);
            Append ("references", Image (References));
            SU.Append (Result, '}');
         end;
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
                  Append ("source", Image (Natural (Handle) - 1), True);
                  Append ("target", Image (Natural (Index  - 1)));
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
                  Append ("source", Image (Natural (Index  - 1)), True);
                  Append ("target", Image (Natural (Handle) - 1));
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
