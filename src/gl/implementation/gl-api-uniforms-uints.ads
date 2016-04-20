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

package GL.API.Uniforms.UInts is
   pragma Preelaborate;

   use GL.Types.UInts;

   procedure Uniform1 is new Loader.Procedure_With_3_Params
     ("glProgramUniform1ui", UInt, Int, UInt);

   procedure Uniform1v is new Loader.Procedure_With_4_Params
     ("glProgramUniform1uiv", UInt, Int, Size, UInt_Array);

   procedure Uniform2 is new Loader.Procedure_With_4_Params
     ("glProgramUniform2ui", UInt, Int, UInt, UInt);

   procedure Uniform2v is new Loader.Procedure_With_4_Params
     ("glProgramUniform2uiv", UInt, Int, Size, Vector2_Array);

   procedure Uniform3 is new Loader.Procedure_With_5_Params
     ("glProgramUniform3ui", UInt, Int, UInt, UInt, UInt);

   procedure Uniform3v is new Loader.Procedure_With_4_Params
     ("glProgramUniform3uiv", UInt, Int, Size, Vector3_Array);

   procedure Uniform4 is new Loader.Procedure_With_6_Params
     ("glProgramUniform4ui", UInt, Int, UInt, UInt, UInt, UInt);

   procedure Uniform4v is new Loader.Procedure_With_4_Params
     ("glProgramUniform4uiv", UInt, Int, Size, Vector4_Array);

end GL.API.Uniforms.UInts;
