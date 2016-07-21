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

with GL.Files;

package body Orka.Programs.Modules is

   procedure Load_And_Compile (Shader : in out GL.Objects.Shaders.Shader; File_Name : String) is
   begin
      if File_Name /= "" then
         Shader.Initialize_Id;
         GL.Files.Load_Shader_Source_From_File (Shader, File_Name);

         Shader.Compile;
         if not Shader.Compile_Status then
            raise Shader_Compile_Error with File_Name & ":" & Shader.Info_Log;
         end if;
      end if;
   end Load_And_Compile;

   function Create_Module (VS, TCS, TES, GS, FS : String := "")
     return Module is
   begin
      return Result : Module do
         Load_And_Compile (Result.Vertex_Shader, VS);
         Load_And_Compile (Result.Tess_Control_Shader, TCS);
         Load_And_Compile (Result.Tess_Evaluation_Shader, TES);
         Load_And_Compile (Result.Geometry_Shader, GS);
         Load_And_Compile (Result.Fragment_Shader, FS);
      end return;
   end Create_Module;

   procedure Attach_Shaders (Modules : Module_Array; Program : Programs.Program) is
      procedure Attach (Shader : GL.Objects.Shaders.Shader) is
      begin
         if Shader.Initialized then
            Program.GL_Program.Attach (Shader);
         end if;
      end Attach;
   begin
      for Module of Modules loop
         Attach (Module.Vertex_Shader);
         Attach (Module.Tess_Control_Shader);
         Attach (Module.Tess_Evaluation_Shader);
         Attach (Module.Geometry_Shader);
         Attach (Module.Fragment_Shader);
      end loop;
   end Attach_Shaders;

end Orka.Programs.Modules;
