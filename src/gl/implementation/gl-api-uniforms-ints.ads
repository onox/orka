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
