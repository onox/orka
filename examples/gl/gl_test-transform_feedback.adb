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

with GL.Attributes;
with GL.Drawing;
with GL.Files;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Objects.Queries;
with GL.Objects.Transform_Feedbacks;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
with GL.Types;

with GL_Test.Display_Backend;

procedure GL_Test.Transform_Feedback is
   Initialized : constant Boolean := Display_Backend.Init
     (Major => 3, Minor => 2, Width => 500, Height => 500, Resizable => False);
   pragma Unreferenced (Initialized);

   use GL.Types;
   use GL.Objects.Queries;
   use GL.Objects.Transform_Feedbacks;
   use GL.Objects.Vertex_Arrays;

   package Single_Pointers is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   procedure Load_Data (Array_Input  : Vertex_Array_Object;
                        Buffer_Input, Buffer_Output : GL.Objects.Buffers.Buffer;
                        Program      : GL.Objects.Programs.Program) is
      use GL.Objects.Buffers;
      use GL.Attributes;

      Vertices : constant Single_Array := (1.0, 2.0, 3.0, 4.0, 5.0);

      Attrib_Pos : constant Attribute :=
        GL.Objects.Programs.Attrib_Location (Program, "in_value");
   begin
      --  Upload Vertices data to Buffer_Input
      Single_Pointers.Load_To_Immutable_Buffer (Buffer_Input, Vertices, Storage_Bits'(others => False));

      -- Enable and set attributes for Array_Input VAO
      Array_Input.Enable_Attribute (Attrib_Pos);

      Array_Input.Set_Attribute_Format (Attrib_Pos, 1, Single_Type, 0);
      Array_Input.Set_Attribute_Binding (Attrib_Pos, 0);

      Array_Input.Bind_Vertex_Buffer (0, Buffer_Input, Single_Type, 0, 1);

      --  Allocate data for Buffer_Output
      Buffer_Output.Allocate (3 * Vertices'Length, Single_Type, Storage_Bits'(others => False));
   end Load_Data;

   procedure Load_Shaders (Vertex_Source, Geometry_Source : String;
                           Vertex_Shader, Geometry_Shader : GL.Objects.Shaders.Shader;
                           Program : GL.Objects.Programs.Program;
                           Outputs : String_Array) is
   begin
      -- Load shader sources and compile shaders
      GL.Files.Load_Shader_Source_From_File
        (Vertex_Shader, Vertex_Source);
      GL.Files.Load_Shader_Source_From_File
        (Geometry_Shader, Geometry_Source);

      Vertex_Shader.Compile;
      Geometry_Shader.Compile;

      if not Vertex_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. Log:");
         Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
      end if;
      if not Geometry_Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of geometry shader failed. Log:");
         Ada.Text_IO.Put_Line (Geometry_Shader.Info_Log);
      end if;

      -- Set up program
      Program.Attach (Vertex_Shader);
      Program.Attach (Geometry_Shader);
      Program.Set_Feedback_Outputs (Outputs, Interleaved_Attributes);

      Program.Link;
      if not Program.Link_Status then
         Ada.Text_IO.Put_Line ("Program linking failed. Log:");
         Ada.Text_IO.Put_Line (Program.Info_Log);
         return;
      end if;
   end Load_Shaders;

   Vertex_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Vertex_Shader);
   Geometry_Shader : GL.Objects.Shaders.Shader
     (Kind => GL.Objects.Shaders.Geometry_Shader);
   Program : GL.Objects.Programs.Program;

   Vertex_Buffer_Input, Vertex_Buffer_Output : GL.Objects.Buffers.Buffer;
   Array_Input : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

   Query : GL.Objects.Queries.Query (Transform_Feedback_Primitives_Written);
   Feedback : GL.Objects.Transform_Feedbacks.Feedback_Object;

   Vertex_Source   : constant String := "../examples/gl/shaders/transform_feedback.vert";
   Geometry_Source : constant String := "../examples/gl/shaders/transform_feedback.geom";

   Feedback_Output : aliased String := "out_value";
   Feedback_Outputs : constant String_Array := (1 => Feedback_Output'Unchecked_Access);
begin
   -- Compile shaders and attach them to the programs
   Load_Shaders (Vertex_Source, Geometry_Source, Vertex_Shader, Geometry_Shader, Program, Feedback_Outputs);
   Program.Use_Program;

   Ada.Text_IO.Put_Line ("Loaded shaders");

   -- Upload vertices to GPU
   Load_Data (Array_Input, Vertex_Buffer_Input, Vertex_Buffer_Output, Program);

   Ada.Text_IO.Put_Line ("Loaded data");

   GL.Toggles.Enable (GL.Toggles.Rasterizer_Discard);

   --  Bind the Vertex_Buffer_Output to index 0 of the transform feedback target
   GL.Objects.Transform_Feedbacks.Active_Transform_Feedback.Bind (Feedback);
   GL.Objects.Buffers.Transform_Feedback_Buffer.Bind (Vertex_Buffer_Output);
   Feedback.Bind_Base (Vertex_Buffer_Output, 0);

   Array_Input.Bind;
   declare
      Q : Active_Query'Class := Query.Begin_Query (Transform_Feedback_Primitives_Written);
      pragma Unreferenced (Q);

      F : Active_Feedback'Class := Feedback.Begin_Feedback (Triangles);
      pragma Unreferenced (F);
   begin
      GL.Drawing.Draw_Arrays (Points, 0, 5);
   end;

   GL.Toggles.Disable (GL.Toggles.Rasterizer_Discard);
   GL.Flush;

   --  Print query result; number of primitives written to the feedback stream
   Ada.Text_IO.Put_Line (Natural'Image (Query.Result) & " primitives written");

   --  Copy the data (5 primitives, each generating 3 elements) from the
   --  bound feedback buffer (Vertex_Buffer_Output) to an array
   declare
      Data : Single_Array (1 .. 15) := (others => 0.0);
   begin
      Single_Pointers.Get_Sub_Data (Vertex_Buffer_Output, 0, Data);
      for Element of Data loop
         Ada.Text_IO.Put_Line (Single'Image (Element));
      end loop;
   end;
end GL_Test.Transform_Feedback;
