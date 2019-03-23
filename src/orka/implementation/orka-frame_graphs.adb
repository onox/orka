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
      Handle  : out Positive)
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
      Handle : Positive;
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

         if Pass.Write_Count > 0 then
            if Pass.Write_Offset + Pass.Write_Count /= Graph.Write_Handles.Length then
               raise Program_Error with
                 "Cannot interleave Add_Output calls for different passes";
            end if;
         else
            Pass.Write_Offset := Handle;
         end if;
         Pass.Write_Count := Pass.Write_Count + 1;
      end;
   end Add_Output;

   procedure Add_Input
     (Object  : Render_Pass;
      Subject : Resource;
      Read    : Read_Mode)
   is
      Handle : Positive;
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
            Pass.Read_Offset := Handle;
         end if;
         Pass.Read_Count := Pass.Read_Count + 1;

         Resource.Read_Count := Resource.Read_Count + 1;
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

   function Cull (Object : Builder) return Graph'Class is
      Stack : Handle_Vectors.Vector (Object.Resources.Length);
      Index : Positive;
   begin
      return Result : Graph (Object.Maximum_Passes, Object.Maximum_Resources) do
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
               pragma Assert (Index in Write_Offset .. Write_Offset + Write_Count - 1);
            begin
               Pass.References := Pass.References - 1;

               --  Update ref count of resources read by the render pass
               --  if the render pass got culled
               if not Pass.Side_Effect and Pass.References = 0 then
                  for Index in Pass.Read_Offset .. Pass.Read_Offset + Pass.Read_Count - 1 loop
                     declare
                        Resource : Resource_Data renames Result.Graph.Resources (Index);
                     begin
                        Resource.References := Resource.References - 1;

                        if Resource.References = 0 then
                           Stack.Append (Index);
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end return;
   end Cull;

   procedure Render (Object : Graph) is
   begin
      for Pass of Object.Graph.Passes loop
         if Pass.Side_Effect or else Pass.References > 0 then
            Pass.Execute (Pass);
         end if;
      end loop;
   end Render;

   ----------------------------------------------------------------------

   procedure Write_Graph
     (Object   : Graph;
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
            for Handle in Pass.Read_Offset .. Pass.Read_Offset + Pass.Read_Count - 1 loop
               Append_Comma;
               SU.Append (Result, '{');
               Append ("source", Image (Handle - 1), True);
               Append ("target", Image (Index  - 1));
               SU.Append (Result, '}');
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
            for Handle in Pass.Write_Offset .. Pass.Write_Offset + Pass.Write_Count - 1 loop
               Append_Comma;
               SU.Append (Result, '{');
               Append ("source", Image (Index  - 1), True);
               Append ("target", Image (Handle - 1));
               SU.Append (Result, '}');
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
