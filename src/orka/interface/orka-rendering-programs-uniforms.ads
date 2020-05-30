--  SPDX-License-Identifier: Apache-2.0
--
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
with GL.Pixels.Extensions;
with GL.Types;

with Orka.Rendering.Textures;

with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Doubles.Matrices;

package Orka.Rendering.Programs.Uniforms is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;
   package PE renames GL.Pixels.Extensions;

   use type LE.Texture_Kind;
   use type LE.Resource_Type;
   use type PE.Format_Type;

   package TS renames Transforms.Singles.Matrices;
   package TD renames Transforms.Doubles.Matrices;

   -----------------------------------------------------------------------------

   function Texture_Kind (Sampler : LE.Resource_Type) return LE.Texture_Kind;
   function Image_Kind   (Image   : LE.Resource_Type) return LE.Texture_Kind;

   function Sampler_Format_Type (Sampler : LE.Resource_Type) return PE.Format_Type;
   function Image_Format_Type   (Image   : LE.Resource_Type) return PE.Format_Type;

   -----------------------------------------------------------------------------

   type Uniform (Kind : LE.Resource_Type) is tagged private;

   procedure Set_Matrix (Object : Uniform; Value : TS.Matrix4)
     with Pre => Object.Kind = LE.Single_Matrix4;
   procedure Set_Matrix (Object : Uniform; Value : TD.Matrix4)
     with Pre => Object.Kind = LE.Double_Matrix4;

   procedure Set_Vector (Object : Uniform; Value : TS.Vector4)
     with Pre => Object.Kind = LE.Single_Vec4;
   procedure Set_Vector (Object : Uniform; Value : TD.Vector4)
     with Pre => Object.Kind = LE.Double_Vec4;

   -----------------------------------------------------------------------------

   procedure Set_Vector
     (Object : Uniform;
      Data   : GL.Types.Int_Array)
   with Pre => (case Object.Kind is
                  when LE.Int_Vec2 => Data'Length = 2,
                  when LE.Int_Vec3 => Data'Length = 3,
                  when LE.Int_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error);

   procedure Set_Vector
     (Object : Uniform;
      Data   : GL.Types.UInt_Array)
   with Pre => (case Object.Kind is
                  when LE.UInt_Vec2 => Data'Length = 2,
                  when LE.UInt_Vec3 => Data'Length = 3,
                  when LE.UInt_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error);

   procedure Set_Vector
     (Object : Uniform;
      Data   : GL.Types.Single_Array)
   with Pre => (case Object.Kind is
                  when LE.Single_Vec2 => Data'Length = 2,
                  when LE.Single_Vec3 => Data'Length = 3,
                  when LE.Single_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error);

   -----------------------------------------------------------------------------

   procedure Set_Single (Object : Uniform; Value : GL.Types.Single)
     with Pre => Object.Kind = LE.Single_Type;

   procedure Set_Double (Object : Uniform; Value : GL.Types.Double)
     with Pre => Object.Kind = LE.Double_Type;

   procedure Set_Int (Object : Uniform; Value : GL.Types.Int)
     with Pre => Object.Kind = LE.Int_Type;

   procedure Set_UInt (Object : Uniform; Value : GL.Types.UInt)
     with Pre => Object.Kind = LE.UInt_Type;

   procedure Set_Integer (Object : Uniform; Value : Integer)
     with Pre => Object.Kind = LE.Int_Type;

   procedure Set_Boolean (Object : Uniform; Value : Boolean)
     with Pre => Object.Kind = LE.Bool_Type;

   -----------------------------------------------------------------------------

   type Uniform_Sampler (Kind : LE.Resource_Type) is tagged private;

   procedure Verify_Compatibility
     (Object  : Uniform_Sampler;
      Texture : GL.Objects.Textures.Texture) is null
   with Pre'Class =>
     (Texture.Kind = Texture_Kind (Object.Kind) or else
       raise Constraint_Error with
         "Cannot bind " & Rendering.Textures.Image (Texture) & " to " &
         Texture_Kind (Object.Kind)'Image & " sampler")
     and then
       --  If the texture is a depth texture, the sampler can be a normal or shadow sampler
       --  (The bound Sampler object must have comparison mode enabled iff the sampler in the
       --  shader is a shadow sampler)
       (Texture.Compressed
          or else
            (if PE.Texture_Format_Type (Texture.Internal_Format) = PE.Depth_Type then
               Sampler_Format_Type (Object.Kind) in PE.Depth_Type | PE.Float_Or_Normalized_Type
             else
               Sampler_Format_Type (Object.Kind) = PE.Texture_Format_Type (Texture.Internal_Format))
          or else raise Constraint_Error with
            "Cannot bind " & Rendering.Textures.Image (Texture) & " to " &
            Object.Kind'Image & " sampler");

   -----------------------------------------------------------------------------

   type Uniform_Image (Kind : LE.Resource_Type) is tagged private;

   procedure Verify_Compatibility
     (Object  : Uniform_Image;
      Texture : GL.Objects.Textures.Texture) is null
   with Pre'Class =>
     (Texture.Kind = Image_Kind (Object.Kind) or else
       raise Constraint_Error with
         "Cannot bind " & Rendering.Textures.Image (Texture) & " to " &
         Image_Kind (Object.Kind)'Image & " image sampler")
     and then
       --  If the texture is a depth texture, the sampler can be a normal or shadow sampler
       --  (The bound Sampler object must have comparison mode enabled iff the sampler in the
       --  shader is a shadow sampler)
       (Texture.Compressed
          or else
            Sampler_Format_Type (Object.Kind) = PE.Image_Format_Type (Texture.Internal_Format)
          or else raise Constraint_Error with
            "Cannot bind " & Rendering.Textures.Image (Texture) & " to " &
            Object.Kind'Image & " image sampler");

   -----------------------------------------------------------------------------

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

   -----------------------------------------------------------------------------

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

   function Create_Uniform_Variable
     (Object : Program;
      Name   : String) return Uniform;

   Uniform_Inactive_Error : exception renames GL.Objects.Programs.Uniform_Inactive_Error;

   Uniform_Type_Error : exception;

private

   type Uniform (Kind : LE.Resource_Type) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   type Uniform_Sampler (Kind : LE.Resource_Type) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   type Uniform_Image (Kind : LE.Resource_Type) is tagged record
      GL_Uniform : GL.Objects.Programs.Uniforms.Uniform;
   end record;

   package Indices_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.Programs.Subroutine_Index_Array,
      "=" => GL.Objects.Programs."=");

   type Uniform_Subroutine (Program : access Programs.Program) is tagged limited record
      Location : Uniform_Location;
      Indices  : Indices_Holder.Holder;
      Shader   : GL.Objects.Shaders.Shader_Type;
   end record;

end Orka.Rendering.Programs.Uniforms;
