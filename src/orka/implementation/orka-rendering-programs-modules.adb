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

package body Orka.Rendering.Programs.Modules is

   procedure Load_And_Compile
     (Object : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      File_Name : String) is
   begin
      if File_Name /= "" then
         pragma Assert (Object.Shaders (Shader_Kind).Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
         begin
            GL.Files.Load_Shader_Source_From_File (Shader, File_Name);

            Shader.Compile;
            if not Shader.Compile_Status then
               raise Shader_Compile_Error with File_Name & ":" & Shader.Info_Log;
            end if;

            Object.Shaders (Shader_Kind).Replace_Element (Shader);
         end;
      end if;
   end Load_And_Compile;

   procedure Set_And_Compile
     (Object : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Source : String) is
   begin
      if Source /= "" then
         pragma Assert (Object.Shaders (Shader_Kind).Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
         begin
            Shader.Set_Source (Source);

            Shader.Compile;
            if not Shader.Compile_Status then
               raise Shader_Compile_Error with Shader_Kind'Image & ":" & Shader.Info_Log;
            end if;

            Object.Shaders (Shader_Kind).Replace_Element (Shader);
         end;
      end if;
   end Set_And_Compile;

   function Create_Module_From_Sources (VS, TCS, TES, GS, FS : String := "")
     return Module
   is
      use GL.Objects.Shaders;
   begin
      return Result : Module do
         Set_And_Compile (Result, Vertex_Shader, VS);
         Set_And_Compile (Result, Tess_Control_Shader, TCS);
         Set_And_Compile (Result, Tess_Evaluation_Shader, TES);
         Set_And_Compile (Result, Geometry_Shader, GS);
         Set_And_Compile (Result, Fragment_Shader, FS);
      end return;
   end Create_Module_From_Sources;

   function Create_Module (VS, TCS, TES, GS, FS : String := "")
     return Module
   is
      use GL.Objects.Shaders;
   begin
      return Result : Module do
         Load_And_Compile (Result, Vertex_Shader, VS);
         Load_And_Compile (Result, Tess_Control_Shader, TCS);
         Load_And_Compile (Result, Tess_Evaluation_Shader, TES);
         Load_And_Compile (Result, Geometry_Shader, GS);
         Load_And_Compile (Result, Fragment_Shader, FS);
      end return;
   end Create_Module;

   procedure Attach_Shaders (Modules : Module_Array; Program : Programs.Program) is
      use GL.Objects.Shaders;

      procedure Attach (Holder : Shader_Holder.Holder) is
      begin
         if not Holder.Is_Empty then
            Program.GL_Program.Attach (Holder.Element);
         end if;
      end Attach;
   begin
      for Module of Modules loop
         Attach (Module.Shaders (Vertex_Shader));
         Attach (Module.Shaders (Tess_Control_Shader));
         Attach (Module.Shaders (Tess_Evaluation_Shader));
         Attach (Module.Shaders (Geometry_Shader));
         Attach (Module.Shaders (Fragment_Shader));
      end loop;
   end Attach_Shaders;

   procedure Detach_Shaders (Modules : Module_Array; Program : Programs.Program) is
      use GL.Objects.Shaders;

      procedure Detach (Holder : Shader_Holder.Holder) is
      begin
         if not Holder.Is_Empty then
            Program.GL_Program.Detach (Holder.Element);
         end if;
      end Detach;
   begin
      for Module of Modules loop
         Detach (Module.Shaders (Vertex_Shader));
         Detach (Module.Shaders (Tess_Control_Shader));
         Detach (Module.Shaders (Tess_Evaluation_Shader));
         Detach (Module.Shaders (Geometry_Shader));
         Detach (Module.Shaders (Fragment_Shader));
      end loop;
   end Detach_Shaders;

end Orka.Rendering.Programs.Modules;
