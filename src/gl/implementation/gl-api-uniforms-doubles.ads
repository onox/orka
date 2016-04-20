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

package GL.API.Uniforms.Doubles is
   pragma Preelaborate;

   use GL.Types.Doubles;

   procedure Uniform1 is new Loader.Procedure_With_3_Params
     ("glProgramUniform1d", UInt, Int, Double);

   procedure Uniform1v is new Loader.Procedure_With_4_Params
     ("glProgramUniform1dv", UInt, Int, Size, Double_Array);

   procedure Uniform2 is new Loader.Procedure_With_4_Params
     ("glProgramUniform2d", UInt, Int, Double, Double);

   procedure Uniform2v is new Loader.Procedure_With_4_Params
     ("glProgramUniform2dv", UInt, Int, Size, Vector2_Array);

   procedure Uniform3 is new Loader.Procedure_With_5_Params
     ("glProgramUniform3d", UInt, Int, Double, Double, Double);

   procedure Uniform3v is new Loader.Procedure_With_4_Params
     ("glProgramUniform3dv", UInt, Int, Size, Vector3_Array);

   procedure Uniform4 is new Loader.Procedure_With_6_Params
     ("glProgramUniform4d", UInt, Int, Double, Double, Double, Double);

   procedure Uniform4v is new Loader.Procedure_With_4_Params
     ("glProgramUniform4dv", UInt, Int, Size, Vector4_Array);    

   procedure Uniform_Matrix2 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix2dv", UInt, Int, Size, Low_Level.Bool,
      Matrix2_Array);

   procedure Uniform_Matrix3 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix3dv", UInt, Int, Size, Low_Level.Bool,
      Matrix3_Array);

   procedure Uniform_Matrix4 is new Loader.Procedure_With_5_Params
     ("glProgramUniformMatrix4dv", UInt, Int, Size, Low_Level.Bool,
      Matrix4_Array);

end GL.API.Uniforms.Doubles;
