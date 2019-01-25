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

with Ada.Unchecked_Conversion;

package body Orka.Rendering.Programs.Uniforms is

   function Texture_Kind (Sampler : LE.Resource_Type) return LE.Texture_Kind is
      use LE;
   begin
      case Sampler is
         when Sampler_1D | Int_Sampler_1D | UInt_Sampler_1D | Sampler_1D_Shadow =>
            return Texture_1D;
         when Sampler_2D | Int_Sampler_2D | UInt_Sampler_2D | Sampler_2D_Shadow =>
            return Texture_2D;
         when Sampler_3D | Int_Sampler_3D | UInt_Sampler_3D =>
            return Texture_3D;
         when Sampler_2D_Rect | Int_Sampler_2D_Rect | UInt_Sampler_2D_Rect | Sampler_2D_Rect_Shadow =>
            return Texture_Rectangle;
         when Sampler_Cube | Int_Sampler_Cube | UInt_Sampler_Cube | Sampler_Cube_Shadow =>
            return Texture_Cube_Map;
         when Sampler_Cube_Array | Int_Sampler_Cube_Array | UInt_Sampler_Cube_Array | Sampler_Cube_Array_Shadow =>
            return Texture_Cube_Map_Array;
         when Sampler_1D_Array | Int_Sampler_1D_Array | UInt_Sampler_1D_Array | Sampler_1D_Array_Shadow =>
            return Texture_1D_Array;
         when Sampler_2D_Array | Int_Sampler_2D_Array | UInt_Sampler_2D_Array | Sampler_2D_Array_Shadow =>
            return Texture_2D_Array;
         when Sampler_Buffer | Int_Sampler_Buffer | UInt_Sampler_Buffer =>
            return Texture_Buffer;
         when Sampler_2D_Multisample | Int_Sampler_2D_Multisample | UInt_Sampler_2D_Multisample =>
            return Texture_2D_Multisample;
         when Sampler_2D_Multisample_Array | Int_Sampler_2D_Multisample_Array | UInt_Sampler_2D_Multisample_Array =>
            return Texture_2D_Multisample_Array;
         when others =>
            raise Constraint_Error;
      end case;
   end Texture_Kind;

   function Texture_Image_Kind (Image : LE.Resource_Type) return LE.Texture_Kind is
      use LE;
   begin
      case Image is
         when Image_1D | Int_Image_1D | UInt_Image_1D =>
            return Texture_1D;
         when Image_2D | Int_Image_2D | UInt_Image_2D =>
            return Texture_2D;
         when Image_3D | Int_Image_3D | UInt_Image_3D =>
            return Texture_3D;
         when Image_2D_Rect | Int_Image_2D_Rect | UInt_Image_2D_Rect =>
            return Texture_Rectangle;
         when Image_Cube | Int_Image_Cube | UInt_Image_Cube =>
            return Texture_Cube_Map;
         when Image_Cube_Map_Array | Int_Image_Cube_Map_Array | UInt_Image_Cube_Map_Array =>
            return Texture_Cube_Map_Array;
         when Image_Buffer | Int_Image_Buffer | UInt_Image_Buffer =>
            return Texture_Buffer;
         when Image_1D_Array | Int_Image_1D_Array | UInt_Image_1D_Array =>
            return Texture_1D_Array;
         when Image_2D_Array | Int_Image_2D_Array | UInt_Image_2D_Array =>
            return Texture_2D_Array;
         when Image_2D_Multisample | Int_Image_2D_Multisample | UInt_Image_2D_Multisample =>
            return Texture_2D_Multisample;
         when Image_2D_Multisample_Array | Int_Image_2D_Multisample_Array | UInt_Image_2D_Multisample_Array =>
            return Texture_2D_Multisample_Array;
         when others =>
            raise Constraint_Error;
      end case;
   end Texture_Image_Kind;

   procedure Set_Matrix (Object : Uniform; Value : TS.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TS.Matrix4, Target => GL.Types.Singles.Matrix4);
   begin
      Object.GL_Uniform.Set_Single_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Matrix (Object : Uniform; Value : TD.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TD.Matrix4, Target => GL.Types.Doubles.Matrix4);
   begin
      Object.GL_Uniform.Set_Double_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Vector (Object : Uniform; Value : TS.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TS.Vector4, Target => GL.Types.Singles.Vector4);
   begin
      Object.GL_Uniform.Set_Single_Vector (Convert (Value));
   end Set_Vector;

   procedure Set_Vector (Object : Uniform; Value : TD.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TD.Vector4, Target => GL.Types.Doubles.Vector4);
   begin
      Object.GL_Uniform.Set_Double_Vector (Convert (Value));
   end Set_Vector;

   procedure Set_Single (Object : Uniform; Value : GL.Types.Single) is
   begin
      Object.GL_Uniform.Set_Single (Value);
   end Set_Single;

   procedure Set_Double (Object : Uniform; Value : GL.Types.Double) is
   begin
      Object.GL_Uniform.Set_Double (Value);
   end Set_Double;

   procedure Set_Int (Object : Uniform; Value : GL.Types.Int) is
   begin
      Object.GL_Uniform.Set_Int (Value);
   end Set_Int;

   procedure Set_UInt (Object : Uniform; Value : GL.Types.UInt) is
   begin
      Object.GL_Uniform.Set_UInt (Value);
   end Set_UInt;

   procedure Set_Boolean (Object : Uniform; Value : Boolean) is
   begin
      Object.GL_Uniform.Set_Int ((if Value then 1 else 0));
   end Set_Boolean;

   procedure Set_Texture
     (Object  : Uniform_Sampler;
      Texture : GL.Objects.Textures.Texture_Base'Class;
      Binding : Natural) is
   begin
      Object.GL_Uniform.Set_Int (GL.Types.Int (Binding));
      Texture.Bind_Texture_Unit (GL.Types.UInt (Binding));
      --  TODO Need to rebind after changing the program
   end Set_Texture;

   procedure Set_Image
     (Object  : Uniform_Image;
      Texture : GL.Objects.Textures.Texture_Base'Class;
      Binding : Natural) is
   begin
      Object.GL_Uniform.Set_Int (GL.Types.Int (Binding));
      Texture.Bind_Image_Texture (GL.Types.UInt (Binding));
      --  TODO Need to rebind after changing the program
   end Set_Image;

   function Is_Compatible
     (Object : Uniform_Subroutine;
      Index  : Subroutine_Index) return Boolean
   is
      use type GL.Types.UInt;
   begin
      return (for some Function_Index of Object.Indices.Element => Index = Function_Index);
   end Is_Compatible;

   procedure Set_Function
     (Object : Uniform_Subroutine;
      Name   : String) is
   begin
      Object.Set_Function (Object.Index (Name));
   end Set_Function;

   procedure Set_Function
     (Object : Uniform_Subroutine;
      Index  : Subroutine_Index) is
   begin
      Object.Program.Set_Subroutine_Function (Object.Shader, Object.Location, Index);
   end Set_Function;

   function Index
     (Object : Uniform_Subroutine;
      Name   : String) return Subroutine_Index is
   begin
      return Object.Program.GL_Program.Subroutine_Index (Object.Shader, Name);
   end Index;

   function Create_Uniform_Sampler
     (Object : Program;
      Name   : String) return Uniform_Sampler
   is
      Sampler_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
   begin
      --  TODO Or store Sampler_Kind?
      return Uniform_Sampler'
        (Kind       => Texture_Kind (Sampler_Kind),
         GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   exception
      when Constraint_Error =>
         raise Uniform_Type_Error with
           "Uniform " & Name & " has unexpected sampler type " & Sampler_Kind'Image;
   end Create_Uniform_Sampler;

   function Create_Uniform_Image
     (Object : Program;
      Name   : String) return Uniform_Image
   is
      Image_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
   begin
      return Uniform_Image'
        (Kind       => Texture_Image_Kind (Image_Kind),
         GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   exception
      when Constraint_Error =>
         raise Uniform_Type_Error with
           "Uniform " & Name & " has unexpected image type " & Image_Kind'Image;
   end Create_Uniform_Image;

   function Create_Uniform_Subroutine
     (Object : in out Program;
      Shader : GL.Objects.Shaders.Shader_Type;
      Name   : String) return Uniform_Subroutine
   is
      Index : constant GL.Objects.Programs.Subroutine_Index_Type
        := Object.GL_Program.Subroutine_Uniform_Index (Shader, Name);

      Indices : constant GL.Objects.Programs.Subroutine_Index_Array
        := Object.GL_Program.Subroutine_Indices_Uniform (Shader, Index);
   begin
      return Uniform_Subroutine'
        (Location => Object.GL_Program.Subroutine_Uniform_Location (Shader, Name),
         Indices  => Indices_Holder.To_Holder (Indices),
         Program  => Object'Access,
         Shader   => Shader);
   end Create_Uniform_Subroutine;

   function Create_Uniform_Block
     (Object : Program;
      Name   : String) return Uniform_Block is
   begin
      return Uniform_Block'
        (GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   end Create_Uniform_Block;

   function Create_Uniform_Variable
     (Object : Program;
      Name   : String) return Uniform
   is
      Uniform_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
   begin
      return Uniform'
        (Kind       => Uniform_Kind,
         GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   end Create_Uniform_Variable;

end Orka.Rendering.Programs.Uniforms;
