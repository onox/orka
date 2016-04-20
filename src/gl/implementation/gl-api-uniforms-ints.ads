--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package GL.API.Uniforms.Ints is
   pragma Preelaborate;

   use GL.Types.Ints;

   procedure Uniform1 is new Loader.Procedure_With_3_Params
     ("glProgramUniform1i", UInt, Int, Int);

   procedure Uniform1v is new Loader.Procedure_With_4_Params
     ("glProgramUniform1iv", UInt, Int, Size, Int_Array);

   procedure Uniform2 is new Loader.Procedure_With_4_Params
     ("glProgramUniform2i", UInt, Int, Int, Int);

   procedure Uniform2v is new Loader.Procedure_With_4_Params
     ("glProgramUniform2iv", UInt, Int, Size, Vector2_Array);

   procedure Uniform3 is new Loader.Procedure_With_5_Params
     ("glProgramUniform3i", UInt, Int, Int, Int, Int);

   procedure Uniform3v is new Loader.Procedure_With_4_Params
     ("glProgramUniform3iv", UInt, Int, Size, Vector3_Array);

   procedure Uniform4 is new Loader.Procedure_With_6_Params
     ("glProgramUniform4i", UInt, Int, Int, Int, Int, Int);

   procedure Uniform4v is new Loader.Procedure_With_4_Params
     ("glProgramUniform4iv", UInt, Int, Size, Vector4_Array);

end GL.API.Uniforms.Ints;
