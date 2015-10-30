--------------------------------------------------------------------------------
-- Copyright (c) 2015 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.Objects.Programs;
with GL.Objects.Shaders;

package GL.Objects.Pipelines is
   pragma Preelaborate;

   type Pipeline is new GL_Object with private;

   procedure Use_Program_Stages (Object : Pipeline; Shader : Shaders.Shader_Type;
                                 Program : Programs.Program);

   procedure Set_Active_Program (Object : Pipeline; Program : Programs.Program);

   procedure Bind (Object : Pipeline);

   function Validate (Object : Pipeline) return Boolean;

   function Info_Log (Object : Pipeline) return String;

   overriding
   procedure Initialize_Id (Object : in out Pipeline);

   overriding
   procedure Delete_Id (Object : in out Pipeline);

private

   type Pipeline is new GL_Object with null record;

end GL.Objects.Pipelines;
