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

with GL.Objects.Shaders;

package Orka.Programs.Modules is
   pragma Preelaborate;

   type Module is tagged private;

   type Module_Array is array (Positive range <>) of aliased Module;

   function Create_Module (VS, TCS, TES, GS, FS : String := "")
     return Module;

   procedure Attach_Shaders (Modules : Module_Array; Program : Programs.Program);

   Shader_Compile_Error : exception;

private

   type Module is tagged record
      Vertex_Shader          : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Vertex_Shader);
      Tess_Control_Shader    : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Tess_Control_Shader);
      Tess_Evaluation_Shader : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Tess_Evaluation_Shader);
      Geometry_Shader        : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Geometry_Shader);
      Fragment_Shader        : GL.Objects.Shaders.Shader
        (Kind => GL.Objects.Shaders.Fragment_Shader);
   end record;

end Orka.Programs.Modules;
