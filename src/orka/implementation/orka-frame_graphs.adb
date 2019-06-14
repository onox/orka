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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with GL.Pixels.Extensions;
with GL.Types;

package body Orka.Frame_Graphs is

   function "+" (Value : Name_Strings.Bounded_String) return String is
     (Name_Strings.To_String (Value));

   function "+" (Value : String) return Name_Strings.Bounded_String is
     (Name_Strings.To_Bounded_String (Value));

   function Name (Object : Render_Pass) return String is
     (+Object.Frame_Graph.Passes (Object.Index).Name);

   procedure Add_Resource
     (Object  : in out Builder;
      Subject : Resource;
      Handle  : out Handle_Type)
   is
      use type Name_Strings.Bounded_String;
      Found  : Boolean := False;
   begin
      for Index in 1 .. Object.Resources.Length loop
         declare
            Description : Resource renames Object.Resources (Index).Description;
         begin
            if Subject.Name = Description.Name and Subject.Version = Description.Version then
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

      if not Found then
         Object.Resources.Append
           ((Description => Subject,
             others      => <>));
         Handle := Object.Resources.Length;
      end if;
   end Add_Resource;

   function Add_Pass
     (Object  : in out Builder;
      Name    : String;
      Execute : not null Execute_Callback;
      Side_Effect : Boolean := False) return Render_Pass'Class is
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

   procedure Add_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Write   : Write_Mode)
   is
      Handle : Handle_Type;
      Graph  : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
   begin
      Add_Resource (Graph, Subject, Handle);

      --  Register resource as 'written' by the render pass
      Graph.Write_Handles.Append (Handle);

      declare
         Pass : Render_Pass_Data renames Graph.Passes (Object.Index);
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         if Resource.Render_Pass /= 0 then
            raise Constraint_Error with "Resource '" & (+Resource.Description.Name) & "'" &
              " already written by pass '" & (+Graph.Passes (Resource.Render_Pass).Name) & "'";
         else
            Resource.Render_Pass := Object.Index;
         end if;
         pragma Assert (Resource.Output_Mode = Not_Used);
         Resource.Output_Mode := Write;

         if Pass.Write_Count > 0 then
            if Pass.Write_Offset + Pass.Write_Count /= Graph.Write_Handles.Length then
               raise Program_Error with
                 "Cannot interleave Add_Output calls for different passes";
            end if;
         else
            Pass.Write_Offset := Graph.Write_Handles.Length;
         end if;
         Pass.Write_Count := Pass.Write_Count + 1;
      end;
   end Add_Output;

   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode)
   is
      Handle : Handle_Type;
      Graph  : Orka.Frame_Graphs.Builder renames Object.Frame_Graph.all;
   begin
      Add_Resource (Graph, Subject, Handle);

      --  Register resource as 'read' by the render pass
      Graph.Read_Handles.Append (Handle);

      declare
         Pass : Render_Pass_Data renames Graph.Passes (Object.Index);
         Resource : Resource_Data renames Graph.Resources (Handle);
      begin
         if Pass.Read_Count > 0 then
            if Pass.Read_Offset + Pass.Read_Count /= Graph.Read_Handles.Length then
               raise Program_Error with
                 "Cannot interleave Add_Input calls for different passes";
            end if;
         else
            Pass.Read_Offset := Graph.Read_Handles.Length;
         end if;
         Pass.Read_Count := Pass.Read_Count + 1;

         Resource.Read_Count := Resource.Read_Count + 1;
         pragma Assert (Resource.Input_Mode = Not_Used or Resource.Input_Mode = Read);
         Resource.Input_Mode := Read;
      end;
   end Add_Input;

   function Add_Input_Output
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode;
      Write   : Write_Mode) return Resource
   is
      Next_Subject : Resource := Subject;
   begin
      Object.Add_Input (Subject, Read);
      Next_Subject.Version := (Version => Next_Subject.Version.Version + 1);
      Object.Add_Output (Next_Subject, Write);
      pragma Assert (Next_Subject.Version.Version = Subject.Version.Version + 1);
      return Next_Subject;
   end Add_Input_Output;

   --  TODO Add pointer to default framebuffer as parameter
   function Cull (Object : Builder) return Graph'Class is
      package PE renames GL.Pixels.Extensions;

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
                       "Resource '" & (+Resource.Description.Name) & " not connected";
                  end if;
                  Stack.Append (Index);
               end if;
            end;
         end loop;

         while not Stack.Empty loop
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

         --  TODO Avoid creating new framebuffers every time Cull is called
         for Index in 1 .. Result.Graph.Passes.Length loop
            declare
               Pass : Render_Pass_Data renames Result.Graph.Passes (Index);
               Width, Height : Natural := Natural'First;

               subtype Selector_Type is GL.Buffers.Explicit_Color_Buffer_Selector;
               subtype Buffer_Type   is GL.Buffers.Draw_Buffer_Index;
            begin
               if Pass.Side_Effect or Pass.References > 0 then
                  declare
                     Current_Selector : Selector_Type := Selector_Type'First;
                     Current_Buffer   : Buffer_Type   := Buffer_Type'First;

                     Clear_Buffer : Boolean := False;
                  begin
                     --  Clear input resources read as framebuffer attachments
                     --  which are new (not written by a previous render pass)
                     for Resource of Result.Input_Resources (Pass) loop
                        if Resource.Mode = Framebuffer_Attachment then
                           Clear_Buffer := not Resource.Written;

                           if PE.Depth_Stencil_Format (Resource.Data.Format) then
                              Pass.Clear_Mask.Depth   := Clear_Buffer;
                              Pass.Clear_Mask.Stencil := Clear_Buffer;
                           elsif PE.Depth_Format (Resource.Data.Format) then
                              Pass.Clear_Mask.Depth   := Clear_Buffer;
                           elsif PE.Stencil_Format (Resource.Data.Format) then
                              Pass.Clear_Mask.Stencil := Clear_Buffer;
                           else
                              --  Clear the color buffers, but only those that
                              --  do not have a render pass that writes to it
                              if Clear_Buffer then
                                 Pass.Clear_Buffers  (Current_Buffer) := Current_Selector;
                                 Pass.Clear_Mask.Color := True;
                              end if;

                              Current_Selector := Selector_Type'Succ (Current_Selector);
                              Current_Buffer   := Buffer_Type'Succ (Current_Buffer);
                           end if;
                        end if;
                     end loop;
                  end;

                  declare
                     Current_Selector : Selector_Type := Selector_Type'First;
                     Current_Buffer   : Buffer_Type   := Buffer_Type'First;

                     Invalidate_Buffer : Boolean := False;
                  begin
                     --  Invalidate output attachments that are transcient
                     --  (not read by a subsequent render pass)
                     for Resource of Result.Output_Resources (Pass) loop
                        if Resource.Mode = Framebuffer_Attachment then
                           Invalidate_Buffer := not Resource.Read;

                           if PE.Depth_Stencil_Format (Resource.Data.Format) then
                              Pass.Invalidate_Mask.Depth   := Invalidate_Buffer;
                              Pass.Invalidate_Mask.Stencil := Invalidate_Buffer;
                           elsif PE.Depth_Format (Resource.Data.Format) then
                              Pass.Invalidate_Mask.Depth   := Invalidate_Buffer;
                           elsif PE.Stencil_Format (Resource.Data.Format) then
                              Pass.Invalidate_Mask.Stencil := Invalidate_Buffer;
                           else
                              --  TODO Only invalidate buffers that are not read instead of none
                              --  (procedure Invalidate will invalidate all attached color
                              --  buffers currently. It is not possible yet to only invalidate
                              --  some specific color buffers)
                              if not Invalidate_Buffer then
                                 Pass.Invalidate_Mask.Color := False;
                              end if;

                              --  Even if the resource is not read, the buffer is
                              --  still used as a draw buffer because the shader
                              --  will probably render to it
                              Pass.Render_Buffers (Current_Buffer) := Current_Selector;

                              Current_Selector := Selector_Type'Succ (Current_Selector);
                              Current_Buffer   := Buffer_Type'Succ (Current_Buffer);
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
                  end;

                  if Pass.Write_Count = 0 then
                     pragma Assert (Pass.Side_Effect);
                     --  TODO Use width and height from default FB
                     Width  := 1280;
                     Height := 720;
                  else
                     pragma Assert (Width > 0 and Height > 0);
                  end if;

                  --  TODO Print performance warning if Pass.Clear_Buffers /= Pass.Render_Buffers
                  declare
                     use type GL.Buffers.Color_Buffer_List;
                  begin
                     Pass.Buffers_Equal := Pass.Clear_Buffers = Pass.Render_Buffers;
                  end;

                  Result.Framebuffers.Append
                    ((Index       => Index,
                      Framebuffer => Framebuffer_Holders.To_Holder
                        (Rendering.Framebuffers.Create_Framebuffer
                           (Width  => GL.Types.Size (Width),
                            Height => GL.Types.Size (Height)))));

                  if Pass.Buffers_Equal then
                     declare
                        procedure Set_Buffers
                          (Framebuffer : in out Rendering.Framebuffers.Framebuffer) is
                        begin
                           Framebuffer.Set_Draw_Buffers (Pass.Render_Buffers);
                        end Set_Buffers;
                     begin
                        Result.Framebuffers
                          (Result.Framebuffers.Length).Framebuffer.Update_Element
                             (Set_Buffers'Access);
                     end;
                  end if;
                  --  TODO Attach resources to FBO here or when binding FBO?
                  --  (for Resource of Object.Output_Resources (Pass))
                  --  TODO Check Pre => not FBO.Has_Attachment (Attachment_Point)
               end if;
            end;
         end loop;
      end return;
   end Cull;

   procedure Render (Object : in out Graph) is
   begin
      for Pass_Data of Object.Framebuffers loop
         declare
            Pass : Render_Pass_Data renames Object.Graph.Passes (Pass_Data.Index);
            pragma Assert (Pass.Side_Effect or else Pass.References > 0);

            procedure Execute_Pass (Framebuffer : in out Rendering.Framebuffers.Framebuffer) is
            begin
               Framebuffer.Use_Framebuffer;

               if not Pass.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Pass.Clear_Buffers);
               end if;
               Framebuffer.Clear (Pass.Clear_Mask);

               --  TODO Bind textures and images (or in procedure Cull?)
               --  (for Resource of Object.Input_Resources (Pass))

               if not Pass.Buffers_Equal then
                  Framebuffer.Set_Draw_Buffers (Pass.Render_Buffers);
               end if;
               Pass.Execute (Pass);

               --  Invalidate attachments that are transcient
               --  (not read by a subsequent render pass)
               Framebuffer.Invalidate (Pass.Invalidate_Mask);
            end Execute_Pass;
         begin
            Pass_Data.Framebuffer.Update_Element (Execute_Pass'Access);
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
               Result (Index) := (Mode    => Data.Input_Mode,
                                  Data    => Data.Description,
                                  Written => Data.Render_Pass /= 0);
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
               Result (Index) := (Mode => Data.Output_Mode,
                                  Data => Data.Description,
                                  Read => Data.References > 0);
            end;
         end loop;
      end return;
   end Output_Resources;

   function Name (Pass : Render_Pass_Data) return String is (+Pass.Name);

   function Clear_Mask (Pass : Render_Pass_Data) return GL.Buffers.Buffer_Bits is
     (Pass.Clear_Mask);

   function Invalidate_Mask (Pass : Render_Pass_Data) return GL.Buffers.Buffer_Bits is
     (Pass.Invalidate_Mask);

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
