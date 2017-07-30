--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with GL.Low_Level.Enums;
with GL.Objects.Programs.Uniforms;
with GL.Objects.Textures;
with GL.Types;

with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Doubles.Matrices;

package Orka.Programs.Uniforms is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;

   use type LE.Texture_Kind;
   use type LE.Resource_Type;

   package TS renames Transforms.Singles.Matrices;
   package TD renames Transforms.Doubles.Matrices;

   type Uniform (Kind : LE.Resource_Type) is tagged private;

   procedure Set_Matrix (Object : Uniform; Value : TS.Matrix4);
   procedure Set_Matrix (Object : Uniform; Value : TD.Matrix4);

   procedure Set_Vector (Object : Uniform; Value : TS.Vector4);
   procedure Set_Vector (Object : Uniform; Value : TD.Vector4);

   procedure Set_Single (Object : Uniform; Value : GL.Types.Single);

   procedure Set_Double (Object : Uniform; Value : GL.Types.Double);

   procedure Set_Int (Object : Uniform; Value : GL.Types.Int);

   procedure Set_UInt (Object : Uniform; Value : GL.Types.UInt);

   procedure Set_Boolean (Object : Uniform; Value : Boolean);

   type Uniform_Sampler (Kind : LE.Texture_Kind) is tagged private;

   procedure Set_Texture
     (Object  : Uniform_Sampler;
      Texture : GL.Objects.Textures.Texture'Class;
      Binding : Natural)
   with Pre => Texture.Kind = Object.Kind;
   --  TODO Add pre condition to check Texture.Format matches Sampler_Kind
   --  (see https://www.khronos.org/opengl/wiki/Sampler_(GLSL)#Sampler_types)
   --  Set the binding point of the uniform sampler and bind the
   --  given texture to the corresponding texture unit

   type Uniform_Image (Kind : LE.Texture_Kind) is tagged private;

   procedure Set_Image
     (Object  : Uniform_Image;
      Texture : GL.Objects.Textures.Texture'Class;
      Binding : Natural)
   with Pre => Texture.Kind = Object.Kind;

   type Uniform_Subroutine (<>) is tagged limited private;

   function Is_Compatible
     (Object : Uniform_Subroutine;
      Index  : Subroutine_Index) return Boolean;
   --  Return True if the index is a compatible subroutine function
   --  for the subroutine uniform, False otherwise

   procedure Set_Function
     (Object : Uniform_Subroutine;
      Name   : String)
   with Inline;
   --  Select the given subroutine function to be used by the
   --  subroutine uniform
   --
   --  This procedure is a shortcut for Set_Function (Index (Name)).

   procedure Set_Function
     (Object : Uniform_Subroutine;
      Index  : Subroutine_Index)
   with Pre => Object.Is_Compatible (Index);
   --  Select the given subroutine function to be used by the
   --  subroutine uniform

   function Index
     (Object : Uniform_Subroutine;
      Name   : String) return Subroutine_Index;
   --  Return the index of the subroutine function identified
   --  by the given name

   type Uniform_Block is tagged private;

   function Create_Uniform_Sampler
     (Object : Program;
      Name   : String) return Uniform_Sampler;

   function Create_Uniform_Image
     (Object : Program;
      Name   : String) return Uniform_Image;

   function Create_Uniform_Subroutine
     (Object : in out Program;
      Shader : GL.Objects.Shaders.Shader_Type;
      Name   : String) return Uniform_Subroutine;

   function Create_Uniform_Block
     (Object : Program;
      Name   : String) return Uniform_Block;

   function Create_Uniform_Variable
     (Object : Program;
      Name   : String) return Uniform;

   Uniform_Inactive_Error : exception renames GL.Objects.Programs.Uniform_Inactive_Error;

   Uniform_Type_Error : exception;

private

   type Uniform (Kind : LE.Resource_Type) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   type Uniform_Sampler (Kind : LE.Texture_Kind) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   type Uniform_Image (Kind : LE.Texture_Kind) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   package Indices_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.Programs.Subroutine_Index_Array,
      "=" => GL.Objects.Programs."=");

   type Uniform_Subroutine (Program : access Orka.Programs.Program) is tagged limited record
      Location : Uniform_Location;
      Indices  : Indices_Holder.Holder;
      Shader   : GL.Objects.Shaders.Shader_Type;
   end record;

   type Uniform_Block is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

end Orka.Programs.Uniforms;
