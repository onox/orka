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

with Ada.Containers.Indefinite_Holders;

with GL.Attributes;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types;

limited with Orka.Programs.Modules;
limited with Orka.Programs.Uniforms;

package Orka.Programs is
   pragma Preelaborate;

   subtype Uniform_Location is GL.Objects.Programs.Uniform_Location_Type;
   subtype Subroutine_Index is GL.Objects.Programs.Subroutine_Index_Type;

   type Program is tagged private;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program;

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program;

   function GL_Program (Object : Program) return GL.Objects.Programs.Program
     with Inline;

   procedure Use_Subroutines (Object : in out Program);
   --  Use the selected subroutines in the current program
   --
   --  This procedure only needs to be called if a different
   --  subroutine function has been selected _after_ Use_Program
   --  was called. If you select the subroutine functions before
   --  calling Use_Program, then Use_Subroutines will be called
   --  automatically.

   procedure Use_Program (Object : in out Program);
   --  Use the program during rendering
   --
   --  If one or more shader stages have subroutines, then these are
   --  used as well (that is, Use_Subroutines is called automatically).

   function Attribute_Location
     (Object : Program;
      Name   : String) return GL.Attributes.Attribute;

   function Uniform_Sampler (Object : Program; Name : String) return Uniforms.Uniform_Sampler;
   --  Return the uniform sampler that has the given name
   --
   --  Name must be a GLSL uniform sampler. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached shaders.

   function Uniform_Subroutine
     (Object : in out Program;
      Shader : GL.Objects.Shaders.Shader_Type;
      Name   : String) return Uniforms.Uniform_Subroutine;
   --  Return the uniform subroutine that has the given name
   --
   --  Name must be a GLSL uniform subroutine. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached
   --  shaders.

   function Uniform_Block (Object : Program; Name : String) return Uniforms.Uniform_Block;
   --  Return the uniform block that has the given name
   --
   --  Name must be a GLSL uniform block. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached
   --  shaders.

   function Uniform (Object : Program; Name : String) return Uniforms.Uniform;
   --  Return the uniform that has the given name
   --
   --  Name must be a GLSL uniform. A Uniforms.Uniform_Inactive_Error exception
   --  is raised if the name is not defined in any of the attached shaders.

   Program_Link_Error : exception;

private

   package Subroutines_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Types.UInt_Array,
      "=" => GL.Types."=");

   type Subroutines_Array is array (GL.Objects.Shaders.Shader_Type) of Subroutines_Holder.Holder;

   type Program is tagged record
      GL_Program  : GL.Objects.Programs.Program;
      Subroutines : Subroutines_Array;
      Subroutines_Modified : Boolean := False;
   end record;

   procedure Set_Subroutine_Function
     (Object   : in out Program;
      Shader   : GL.Objects.Shaders.Shader_Type;
      Location : Uniform_Location;
      Index    : Subroutine_Index);

end Orka.Programs;
