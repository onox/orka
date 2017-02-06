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

with GL.Attributes;
with GL.Objects.Programs;

limited with Orka.Programs.Modules;
limited with Orka.Programs.Uniforms;

package Orka.Programs is
   pragma Preelaborate;

   type Program is tagged limited private;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program;

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program;

   function GL_Program (Object : Program) return GL.Objects.Programs.Program
     with Inline;

   procedure Use_Program (Object : Program);

   function Attribute_Location (Object : Program; Name : String)
     return GL.Attributes.Attribute;

   function Uniform_Sampler (Object : Program; Name : String)
     return Programs.Uniforms.Uniform_Sampler;
   --  Return the uniform sampler that has the given name
   --
   --  Name must be a GLSL uniform sampler. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached shaders.

   function Uniform (Object : Program; Name : String)
     return Programs.Uniforms.Uniform;
   --  Return the uniform that has the given name
   --
   --  Name must be a GLSL uniform. A Uniforms.Uniform_Inactive_Error exception
   --  is raised if the name is not defined in any of the attached shaders.

   Program_Link_Error : exception;

private

   type Program is tagged limited record
      GL_Program : GL.Objects.Programs.Program;
   end record;

end Orka.Programs;
