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

package body Orka.Rendering.Programs.Modules is

   procedure Load_And_Compile
     (Object      : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Location    : Resources.Locations.Location_Ptr;
      Path        : String) is
   begin
      if Path /= "" then
         pragma Assert (Object.Shaders (Shader_Kind).Is_Empty);
         declare
            Shader : GL.Objects.Shaders.Shader (Kind => Shader_Kind);
            Source : Resources.Byte_Array_Access := Location.Read_Data (Path);
         begin
            Shader.Set_Source (Resources.Convert (Source));
            Resources.Free (Source);

            Shader.Compile;
            if not Shader.Compile_Status then
               raise Shader_Compile_Error with Path & ":" & Shader.Info_Log;
            end if;

            Object.Shaders (Shader_Kind).Replace_Element (Shader);
         end;
      end if;
   end Load_And_Compile;

   procedure Set_And_Compile
     (Object      : in out Module;
      Shader_Kind : GL.Objects.Shaders.Shader_Type;
      Source      : String) is
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

   function Create_Module_From_Sources (VS, TCS, TES, GS, FS, CS : String := "")
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
         Set_And_Compile (Result, Compute_Shader, CS);
      end return;
   end Create_Module_From_Sources;

   function Create_Module
     (Location : Resources.Locations.Location_Ptr;
      VS, TCS, TES, GS, FS, CS : String := "") return Module
   is
      use GL.Objects.Shaders;
   begin
      return Result : Module do
         Load_And_Compile (Result, Vertex_Shader, Location, VS);
         Load_And_Compile (Result, Tess_Control_Shader, Location, TCS);
         Load_And_Compile (Result, Tess_Evaluation_Shader, Location, TES);
         Load_And_Compile (Result, Geometry_Shader, Location, GS);
         Load_And_Compile (Result, Fragment_Shader, Location, FS);
         Load_And_Compile (Result, Compute_Shader, Location, CS);
      end return;
   end Create_Module;

   procedure Attach_Shaders (Modules : Module_Array; Program : in out Programs.Program) is
      use GL.Objects.Shaders;

      procedure Attach (Subject : Module; Stage : GL.Objects.Shaders.Shader_Type) is
         Holder : Shader_Holder.Holder renames Subject.Shaders (Stage);
      begin
         if not Holder.Is_Empty then
            Program.GL_Program.Attach (Holder.Element);
            Program.Stages (Stage) := True;
         end if;
      end Attach;
   begin
      for Module of Modules loop
         Attach (Module, Vertex_Shader);
         Attach (Module, Tess_Control_Shader);
         Attach (Module, Tess_Evaluation_Shader);
         Attach (Module, Geometry_Shader);
         Attach (Module, Fragment_Shader);
         Attach (Module, Compute_Shader);
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
         Detach (Module.Shaders (Compute_Shader));
      end loop;
   end Detach_Shaders;

end Orka.Rendering.Programs.Modules;
