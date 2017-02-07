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

package body Orka.Framebuffers is

   function Create_Framebuffer return Framebuffer is
   begin
      return Result : Framebuffer do
         null;
      end return;
   end Create_Framebuffer;

   function Default_Framebuffer return Framebuffer is
   begin
      return Framebuffer'(GL_Framebuffer => GL.Objects.Framebuffers.Default_Framebuffer);
   end Default_Framebuffer;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     is (Object.GL_Framebuffer);

   procedure Use_Framebuffer (Object : Framebuffer) is
      use GL.Objects.Framebuffers;
   begin
      GL.Objects.Framebuffers.Draw_Target.Bind (Object.GL_Framebuffer);

      --  Check attachments
      if Object.GL_Framebuffer /= Default_Framebuffer.GL_Framebuffer
        and then Object.GL_Framebuffer.Status /= Complete
      then
         raise Framebuffer_Incomplete_Error with Framebuffer_Status'Image (Object.GL_Framebuffer.Status);
      end if;
   end Use_Framebuffer;

   procedure Draw (Object : Framebuffer; Program : in out Orka.Programs.Program;
                   Mesh : Orka.Meshes.Vertex_Format; Offset, Count : GL.Types.Size) is
   begin
      Use_Framebuffer (Object);
      Program.Use_Program;
      Mesh.Draw (Count, Offset);
   end Draw;

   procedure Draw_Indirect (Object : Framebuffer; Program : in out Orka.Programs.Program;
                            Mesh : Orka.Meshes.Vertex_Format; Buffer : Orka.Buffers.Buffer) is
   begin
      Use_Framebuffer (Object);
      Program.Use_Program;
      Mesh.Draw_Indirect (Buffer);
   end Draw_Indirect;

end Orka.Framebuffers;
