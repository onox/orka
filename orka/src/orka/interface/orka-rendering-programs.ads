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

with Ada.Containers.Indefinite_Holders;

with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Types.Compute;

with Orka.Rendering.Buffers;

limited with Orka.Rendering.Programs.Modules;
limited with Orka.Rendering.Programs.Uniforms;

package Orka.Rendering.Programs is
   pragma Preelaborate;

   subtype Uniform_Location is GL.Objects.Programs.Uniform_Location_Type;
   subtype Subroutine_Index is GL.Objects.Programs.Subroutine_Index_Type;

   type Program is tagged private;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program;

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program;

   function Has_Subroutines (Object : Program) return Boolean;

   procedure Use_Subroutines (Object : in out Program)
     with Pre => Object.Has_Subroutines;
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

   function Compute_Work_Group_Size
     (Object : Program) return GL.Types.Compute.Dimension_Size_Array;

   function Uniform_Sampler (Object : Program; Name : String) return Uniforms.Uniform_Sampler;
   --  Return the uniform sampler that has the given name
   --
   --  This function is only needed in order to call procedure Verify_Compatibility
   --  to verify that the kind and format of the sampler and texture are
   --  compatible.
   --
   --  To bind a texture to a sampler, call Orka.Rendering.Textures.Bind.
   --
   --  Name must be a GLSL uniform sampler. A Uniforms.Uniform_Inactive_Error
   --  exception is raised if the name is not defined in any of the attached shaders.

   function Uniform_Image (Object : Program; Name : String) return Uniforms.Uniform_Image;
   --  Return the uniform image that has the given name
   --
   --  This function is only needed in order to call procedure Verify_Compatibility
   --  to verify that the kind and format of the image sampler and texture are
   --  compatible.
   --
   --  To bind a texture to a image sampler, call Orka.Rendering.Textures.Bind.
   --
   --  Name must be a GLSL uniform image. A Uniforms.Uniform_Inactive_Error
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

   function Uniform (Object : Program; Name : String) return Uniforms.Uniform;
   --  Return the uniform that has the given name
   --
   --  Name must be a GLSL uniform. A Uniforms.Uniform_Inactive_Error exception
   --  is raised if the name is not defined in any of the attached shaders.

   function Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural;
   --  Return the index of the binding point of a shader storage block (SSBO),
   --  uniform block (UBO), or an atomic counter buffer
   --
   --  Name must be a GLSL shader storage block, uniform block, or atomic
   --  uniform. A Uniforms.Uniform_Inactive_Error exception is raised if
   --  the name is not defined in any of the attached shaders.

   Program_Link_Error : exception;

private

   package Subroutines_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Types.UInt_Array,
      "=" => GL.Types."=");

   type Subroutines_Array is array (GL.Objects.Shaders.Shader_Type) of Subroutines_Holder.Holder;

   type Stages_Array is array (GL.Objects.Shaders.Shader_Type) of Boolean;

   type Program is tagged record
      GL_Program  : GL.Objects.Programs.Program;
      Subroutines : Subroutines_Array;
      Stages      : Stages_Array := (others => False);
      Has_Subroutines      : Boolean := False;
      Subroutines_Modified : Boolean := False;
   end record;

   procedure Set_Subroutine_Function
     (Object   : in out Program;
      Shader   : GL.Objects.Shaders.Shader_Type;
      Location : Uniform_Location;
      Index    : Subroutine_Index)
   with Pre => Object.Has_Subroutines and then not Object.Subroutines (Shader).Is_Empty;

end Orka.Rendering.Programs;
