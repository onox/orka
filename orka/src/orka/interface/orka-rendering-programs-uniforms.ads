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

with GL.Low_Level.Enums;
with GL.Objects.Programs.Uniforms;
with GL.Objects.Textures;
with GL.Pixels.Extensions;

with Orka.Rendering.Textures;
with Orka.Types;

package Orka.Rendering.Programs.Uniforms is
   pragma Preelaborate;

   package LE renames GL.Low_Level.Enums;
   package PE renames GL.Pixels.Extensions;

   use type LE.Texture_Kind;
   use type LE.Resource_Type;
   use type PE.Format_Type;

   -----------------------------------------------------------------------------

   function Texture_Kind (Sampler : LE.Resource_Type) return LE.Texture_Kind;
   function Image_Kind   (Image   : LE.Resource_Type) return LE.Texture_Kind;

   function Sampler_Format_Type (Sampler : LE.Resource_Type) return PE.Format_Type;
   function Image_Format_Type   (Image   : LE.Resource_Type) return PE.Format_Type;

   -----------------------------------------------------------------------------

   type Uniform (Kind : LE.Resource_Type) is tagged private;

   procedure Set_Matrix (Object : Uniform; Value : Types.Singles.Matrix4)
     with Pre => Object.Kind = LE.Single_Matrix4;
   procedure Set_Matrix (Object : Uniform; Value : Types.Doubles.Matrix4)
     with Pre => Object.Kind = LE.Double_Matrix4;

   procedure Set_Vector (Object : Uniform; Value : Types.Singles.Vector4)
     with Pre => Object.Kind = LE.Single_Vec4;
   procedure Set_Vector (Object : Uniform; Value : Types.Doubles.Vector4)
     with Pre => Object.Kind = LE.Double_Vec4;

   -----------------------------------------------------------------------------

   procedure Set_Vector
     (Object : Uniform;
      Data   : Integer_32_Array)
   with Pre => (case Object.Kind is
                  when LE.Int_Vec2 => Data'Length = 2,
                  when LE.Int_Vec3 => Data'Length = 3,
                  when LE.Int_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error with "Unexpected vector type " & Object.Kind'Image);

   procedure Set_Vector
     (Object : Uniform;
      Data   : Unsigned_32_Array)
   with Pre => (case Object.Kind is
                  when LE.UInt_Vec2 => Data'Length = 2,
                  when LE.UInt_Vec3 => Data'Length = 3,
                  when LE.UInt_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error with "Unexpected vector type " & Object.Kind'Image);

   procedure Set_Vector
     (Object : Uniform;
      Data   : Float_32_Array)
   with Pre => (case Object.Kind is
                  when LE.Single_Vec2 => Data'Length = 2,
                  when LE.Single_Vec3 => Data'Length = 3,
                  when LE.Single_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error with "Unexpected vector type " & Object.Kind'Image);

   procedure Set_Vector
     (Object : Uniform;
      Data   : Float_64_Array)
   with Pre => (case Object.Kind is
                  when LE.Double_Vec2 => Data'Length = 2,
                  when LE.Double_Vec3 => Data'Length = 3,
                  when LE.Double_Vec4 => Data'Length = 4,
                  when others => raise Constraint_Error with "Unexpected vector type " & Object.Kind'Image);

   -----------------------------------------------------------------------------

   procedure Set_Single (Object : Uniform; Value : Float_32)
     with Pre => Object.Kind = LE.Single_Type;

   procedure Set_Double (Object : Uniform; Value : Float_64)
     with Pre => Object.Kind = LE.Double_Type;

   procedure Set_Int (Object : Uniform; Value : Integer_32)
     with Pre => Object.Kind = LE.Int_Type;

   procedure Set_UInt (Object : Uniform; Value : Unsigned_32)
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
               Sampler_Format_Type (Object.Kind) =
                 PE.Texture_Format_Type (Texture.Internal_Format))
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

   function Create_Uniform_Sampler
     (Object : Program;
      Name   : String) return Uniform_Sampler;

   function Create_Uniform_Image
     (Object : Program;
      Name   : String) return Uniform_Image;

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

end Orka.Rendering.Programs.Uniforms;
