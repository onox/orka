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

   type Uniform_Sampler (Kind : LE.Texture_Kind) is tagged private;

   procedure Set_Texture
     (Object  : Uniform_Sampler;
      Texture : GL.Objects.Textures.Texture'Class;
      Binding : GL.Types.Int)
   with Pre => Texture.Kind = Object.Kind;

   function Create_Uniform_Sampler (Object : Program; Name : String)
     return Uniform_Sampler;

   function Create_Uniform_Variable (Object : Program; Name : String)
     return Uniform;

   Uniform_Inactive_Error : exception renames GL.Objects.Programs.Uniform_Inactive_Error;

   Uniform_Type_Error : exception;

private

   type Uniform (Kind : LE.Resource_Type) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   type Uniform_Sampler (Kind : LE.Texture_Kind) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

end Orka.Programs.Uniforms;
