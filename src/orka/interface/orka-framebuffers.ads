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

with GL.Objects.Framebuffers;
with GL.Types;

with Orka.Buffers;
with Orka.Meshes;
with Orka.Programs;

package Orka.Framebuffers is
   pragma Preelaborate;

   type Framebuffer is tagged limited private;

   function Create_Framebuffer return Framebuffer;

   function Default_Framebuffer return Framebuffer
     with Inline;

   function GL_Framebuffer (Object : Framebuffer) return GL.Objects.Framebuffers.Framebuffer
     with Inline;

   procedure Draw (Object : Framebuffer; Program : in out Orka.Programs.Program;
                   Mesh : Orka.Meshes.Vertex_Format; Offset, Count : GL.Types.Size);

   procedure Draw_Indirect (Object : Framebuffer; Program : in out Orka.Programs.Program;
                            Mesh : Orka.Meshes.Vertex_Format; Buffer : Orka.Buffers.Buffer);

   Framebuffer_Incomplete_Error : exception;

private

   type Framebuffer is tagged limited record
      GL_Framebuffer : GL.Objects.Framebuffers.Framebuffer;
   end record;

end Orka.Framebuffers;
