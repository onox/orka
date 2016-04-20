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

package GL.API.Uniforms.Singles is
   pragma Preelaborate;

   use GL.Types.Singles;

   procedure Uniform1 is new Loader.Procedure_With_3_Params
     ("glProgramUniform1f", UInt, Int, Single);

   procedure Uniform1v is new Loader.Procedure_With_4_Params
     ("glProgramUniform1fv", UInt, Int, Size, Single_Array);

   procedure Uniform2 is new Loader.Procedure_With_4_Params
     ("glProgramUniform2f", UInt, Int, Single, Single);

   procedure Uniform2v is new Loader.Procedure_With_4_Params
     ("glProgramUniform2fv", UInt, Int, Size, Vector2_Array);

   procedure Uniform3 is new Loader.Procedure_With_5_Params
     ("glProgramUniform3f", UInt, Int, Single, Single, Single);

   procedure Uniform3v is new Loader.Procedure_With_4_Params
     ("glProgramUniform3fv", UInt, Int, Size, Vector3_Array);

   procedure Uniform4 is new Loader.Procedure_With_6_Params
     ("glProgramUniform4f", UInt, Int, Single, Single, Single, Single);

   procedure Uniform4v is new Loader.Procedure_With_4_Params
     ("glProgramUniform4fv", UInt, Int, Size, Vector4_Array);    

   procedure Uniform_Matrix2 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix2fv", UInt, Int, Size, Low_Level.Bool,
      Matrix2_Array);

   procedure Uniform_Matrix3 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix3fv", UInt, Int, Size, Low_Level.Bool,
      Matrix3_Array);

   procedure Uniform_Matrix4 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix4fv", UInt, Int, Size, Low_Level.Bool,
      Matrix4_Array);

end GL.API.Uniforms.Singles;
