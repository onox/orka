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

package GL.API.Uniforms.UInts is
   pragma Preelaborate;

   use GL.Types.UInts;

   package Uniform1 is new Loader.Procedure_With_3_Params
     ("glProgramUniform1ui", UInt, Int, UInt);

   package Uniform1v is new Loader.Procedure_With_4_Params
     ("glProgramUniform1uiv", UInt, Int, Size, UInt_Array);

   package Uniform2v is new Loader.Procedure_With_4_Params
     ("glProgramUniform2uiv", UInt, Int, Size, Vector2_Array);

   package Uniform3v is new Loader.Procedure_With_4_Params
     ("glProgramUniform3uiv", UInt, Int, Size, Vector3_Array);

   package Uniform4v is new Loader.Procedure_With_4_Params
     ("glProgramUniform4uiv", UInt, Int, Size, Vector4_Array);

end GL.API.Uniforms.UInts;
