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

with Ada.Text_IO;

with GL.Objects.Queries;
with GL.Objects.Transform_Feedbacks;
with GL.Toggles;
with GL.Types;

with Orka.Debug;
with Orka.Rendering.Buffers;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Vertex_Formats;
with Orka.Resources.Locations.Directories;
with Orka.Types;

with Orka.Windows.GLFW;

procedure Orka_Test.Test_6_Transform_Feedback is
   Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class
     := Orka.Windows.GLFW.Initialize (Major => 4, Minor => 5, Debug => True);
   pragma Unreferenced (Initialized);

   W : aliased Orka.Windows.Window'Class := Orka.Windows.GLFW.Create_Window
     (1, 1, Visible => False);
   pragma Unreferenced (W);

   ----------------------------------------------------------------------

   use GL.Types;
   use GL.Objects.Queries;
   use GL.Objects.Transform_Feedbacks;

   use Orka.Rendering.Buffers;
   use Orka.Rendering.Framebuffers;
   use Orka.Rendering.Programs;
   use Orka.Rendering.Vertex_Formats;
   use Orka.Resources;

   use all type Orka.Types.Element_Type;

   Vertices : constant Single_Array := (1.0, 2.0, 3.0, 4.0, 5.0);

   VBO_1 : constant Buffer := Create_Buffer ((others => False), Single_Type, 3 * Vertices'Length);

   function Load_Data (Program : Orka.Rendering.Programs.Program) return Vertex_Format is
      VBO_2 : constant Buffer := Create_Buffer ((others => False), Vertices);

      procedure Add_Vertex_Attributes (Buffer : in out Attribute_Buffer) is
      begin
         Buffer.Add_Attribute (Program.Attribute_Location ("in_value"), 1);
         Buffer.Set_Buffer (VBO_2);
      end Add_Vertex_Attributes;
   begin
      return Result : Vertex_Format := Create_Vertex_Format (Points, UInt_Type) do
         Result.Add_Attribute_Buffer (Single_Type, Add_Vertex_Attributes'Access);
      end return;
   end Load_Data;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../examples/gl/shaders");

   Program_1 : Program := Create_Program (Modules.Create_Module
     (Location_Shaders, VS => "transform_feedback.vert", GS => "transform_feedback.geom"));

   VF_1 : constant Vertex_Format := Load_Data (Program_1);

   Query    : GL.Objects.Queries.Query (Transform_Feedback_Primitives_Written);
   Feedback : GL.Objects.Transform_Feedbacks.Feedback_Object;
begin
   Orka.Debug.Flush_Log;
   Orka.Debug.Enable_Print_Callback;

   Program_1.Use_Program;

   GL.Toggles.Enable (GL.Toggles.Rasterizer_Discard);

   --  Bind the VBO_1 to index 0 of the transform feedback target
   VBO_1.Bind_Base (Transform_Feedback, 0);

   declare
      Q : Active_Query'Class := Query.Begin_Query;
      pragma Unreferenced (Q);

      F : Active_Feedback'Class := Feedback.Begin_Feedback (Triangles);
      pragma Unreferenced (F);
   begin
      VF_1.Draw (0, 5);
   end;

   GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);
   GL.Flush;

   --  Print query result; number of primitives written to the feedback stream
   Ada.Text_IO.Put_Line (Natural'Image (Query.Result) & " primitives written");

   --  Copy the data (n primitives, each generating 3 elements) from the
   --  bound feedback buffer (VBO_1) to an array
   declare
      Data  : Single_Array (1 .. Size (VBO_1.Length)) := (others => 0.0);
   begin
      VBO_1.Get_Data (Data);
      for Element of Data loop
         Ada.Text_IO.Put_Line (Element'Image);
      end loop;
   end;
end Orka_Test.Test_6_Transform_Feedback;
