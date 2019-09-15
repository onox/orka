--  SPDX-License-Identifier: Apache-2.0
--
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
