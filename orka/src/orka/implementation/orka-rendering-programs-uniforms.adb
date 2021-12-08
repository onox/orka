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
         when Sampler_2D_Rect | Int_Sampler_2D_Rect | UInt_Sampler_2D_Rect |
           Sampler_2D_Rect_Shadow =>
            return Texture_Rectangle;
         when Sampler_Cube | Int_Sampler_Cube | UInt_Sampler_Cube | Sampler_Cube_Shadow =>
            return Texture_Cube_Map;
         when Sampler_Cube_Array | Int_Sampler_Cube_Array | UInt_Sampler_Cube_Array |
           Sampler_Cube_Array_Shadow =>
            return Texture_Cube_Map_Array;
         when Sampler_1D_Array | Int_Sampler_1D_Array | UInt_Sampler_1D_Array |
           Sampler_1D_Array_Shadow =>
            return Texture_1D_Array;
         when Sampler_2D_Array | Int_Sampler_2D_Array | UInt_Sampler_2D_Array |
           Sampler_2D_Array_Shadow =>
            return Texture_2D_Array;
         when Sampler_Buffer | Int_Sampler_Buffer | UInt_Sampler_Buffer =>
            return Texture_Buffer;
         when Sampler_2D_Multisample | Int_Sampler_2D_Multisample | UInt_Sampler_2D_Multisample =>
            return Texture_2D_Multisample;
         when Sampler_2D_Multisample_Array | Int_Sampler_2D_Multisample_Array |
           UInt_Sampler_2D_Multisample_Array =>
            return Texture_2D_Multisample_Array;
         when others =>
            raise Constraint_Error;
      end case;
   end Texture_Kind;

   function Image_Kind (Image : LE.Resource_Type) return LE.Texture_Kind is
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
         when Image_2D_Multisample_Array | Int_Image_2D_Multisample_Array |
           UInt_Image_2D_Multisample_Array =>
            return Texture_2D_Multisample_Array;
         when others =>
            raise Constraint_Error;
      end case;
   end Image_Kind;

   function Sampler_Format_Type (Sampler : LE.Resource_Type) return PE.Format_Type is
      use LE;
      use PE;
   begin
      case Sampler is
         when Sampler_1D | Sampler_2D | Sampler_3D | Sampler_2D_Rect |
           Sampler_Cube | Sampler_Cube_Array | Sampler_Buffer |
           Sampler_1D_Array | Sampler_2D_Array |
           Sampler_2D_Multisample | Sampler_2D_Multisample_Array =>
            return Float_Or_Normalized_Type;
         when Int_Sampler_1D | Int_Sampler_2D | Int_Sampler_3D | Int_Sampler_2D_Rect |
           Int_Sampler_Cube | Int_Sampler_Cube_Array | Int_Sampler_Buffer |
           Int_Sampler_1D_Array | Int_Sampler_2D_Array |
           Int_Sampler_2D_Multisample | Int_Sampler_2D_Multisample_Array =>
            return Int_Type;
         when UInt_Sampler_1D | UInt_Sampler_2D | UInt_Sampler_3D | UInt_Sampler_2D_Rect |
           UInt_Sampler_Cube | UInt_Sampler_Cube_Array | UInt_Sampler_Buffer |
           UInt_Sampler_1D_Array | UInt_Sampler_2D_Array |
           UInt_Sampler_2D_Multisample | UInt_Sampler_2D_Multisample_Array =>
            return Unsigned_Int_Type;
         when Sampler_1D_Shadow | Sampler_2D_Shadow | Sampler_2D_Rect_Shadow |
           Sampler_Cube_Shadow | Sampler_Cube_Array_Shadow |
           Sampler_1D_Array_Shadow | Sampler_2D_Array_Shadow =>
            return Depth_Type;
         when others =>
            raise Constraint_Error;
      end case;
   end Sampler_Format_Type;

   function Image_Format_Type (Image : LE.Resource_Type) return PE.Format_Type is
      use LE;
      use PE;
   begin
      case Image is
         when Image_1D | Image_2D | Image_3D | Image_2D_Rect |
           Image_Cube | Image_Cube_Map_Array | Image_Buffer |
           Image_1D_Array | Image_2D_Array |
           Image_2D_Multisample | Image_2D_Multisample_Array =>
            return Float_Or_Normalized_Type;
         when Int_Image_1D | Int_Image_2D | Int_Image_3D | Int_Image_2D_Rect |
           Int_Image_Cube | Int_Image_Cube_Map_Array | Int_Image_Buffer |
           Int_Image_1D_Array | Int_Image_2D_Array |
           Int_Image_2D_Multisample | Int_Image_2D_Multisample_Array =>
            return Int_Type;
         when UInt_Image_1D | UInt_Image_2D | UInt_Image_3D | UInt_Image_2D_Rect |
           UInt_Image_Cube | UInt_Image_Cube_Map_Array | UInt_Image_Buffer |
           UInt_Image_1D_Array | UInt_Image_2D_Array |
           UInt_Image_2D_Multisample | UInt_Image_2D_Multisample_Array =>
            return Unsigned_Int_Type;
         when others =>
            raise Constraint_Error;
      end case;
   end Image_Format_Type;

   -----------------------------------------------------------------------------

   procedure Set_Matrix (Object : Uniform; Value : Types.Singles.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Singles.Matrix4, Target => GL.Types.Singles.Matrix4);
   begin
      Object.GL_Uniform.Set_Single_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Matrix (Object : Uniform; Value : Types.Doubles.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Doubles.Matrix4, Target => GL.Types.Doubles.Matrix4);
   begin
      Object.GL_Uniform.Set_Double_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Vector (Object : Uniform; Value : Types.Singles.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Singles.Vector4, Target => GL.Types.Singles.Vector4);
   begin
      Object.GL_Uniform.Set_Single_Vector (Convert (Value));
   end Set_Vector;

   procedure Set_Vector (Object : Uniform; Value : Types.Doubles.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Doubles.Vector4, Target => GL.Types.Doubles.Vector4);
   begin
      Object.GL_Uniform.Set_Double_Vector (Convert (Value));
   end Set_Vector;

   -----------------------------------------------------------------------------

   procedure Set_Vector
     (Object : Uniform;
      Data   : Integer_32_Array)
   is
      Offset : constant Size := Data'First;
   begin
      case Data'Length is
         when 2 =>
            Object.GL_Uniform.Set_Int_Vector (GL.Types.Ints.Vector2'
              (Data (Offset), Data (Offset + 1)));
         when 3 =>
            Object.GL_Uniform.Set_Int_Vector (GL.Types.Ints.Vector3'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2)));
         when 4 =>
            Object.GL_Uniform.Set_Int_Vector (GL.Types.Ints.Vector4'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2), Data (Offset + 3)));
         when others => raise Constraint_Error;
      end case;
   end Set_Vector;

   procedure Set_Vector
     (Object : Uniform;
      Data   : Unsigned_32_Array)
   is
      Offset : constant Size := Data'First;
   begin
      case Data'Length is
         when 2 =>
            Object.GL_Uniform.Set_UInt_Vector (GL.Types.UInts.Vector2'
              (Data (Offset), Data (Offset + 1)));
         when 3 =>
            Object.GL_Uniform.Set_UInt_Vector (GL.Types.UInts.Vector3'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2)));
         when 4 =>
            Object.GL_Uniform.Set_UInt_Vector (GL.Types.UInts.Vector4'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2), Data (Offset + 3)));
         when others => raise Constraint_Error;
      end case;
   end Set_Vector;

   procedure Set_Vector
     (Object : Uniform;
      Data   : Float_32_Array)
   is
      Offset : constant Size := Data'First;
   begin
      case Data'Length is
         when 2 =>
            Object.GL_Uniform.Set_Single_Vector (GL.Types.Singles.Vector2'
              (Data (Offset), Data (Offset + 1)));
         when 3 =>
            Object.GL_Uniform.Set_Single_Vector (GL.Types.Singles.Vector3'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2)));
         when 4 =>
            Object.GL_Uniform.Set_Single_Vector (GL.Types.Singles.Vector4'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2), Data (Offset + 3)));
         when others => raise Constraint_Error;
      end case;
   end Set_Vector;

   procedure Set_Vector
     (Object : Uniform;
      Data   : Float_64_Array)
   is
      Offset : constant Size := Data'First;
   begin
      case Data'Length is
         when 2 =>
            Object.GL_Uniform.Set_Double_Vector (GL.Types.Doubles.Vector2'
              (Data (Offset), Data (Offset + 1)));
         when 3 =>
            Object.GL_Uniform.Set_Double_Vector (GL.Types.Doubles.Vector3'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2)));
         when 4 =>
            Object.GL_Uniform.Set_Double_Vector (GL.Types.Doubles.Vector4'
              (Data (Offset), Data (Offset + 1), Data (Offset + 2), Data (Offset + 3)));
         when others => raise Constraint_Error;
      end case;
   end Set_Vector;

   -----------------------------------------------------------------------------

   procedure Set_Single (Object : Uniform; Value : Float_32) is
   begin
      Object.GL_Uniform.Set_Single (Value);
   end Set_Single;

   procedure Set_Double (Object : Uniform; Value : Float_64) is
   begin
      Object.GL_Uniform.Set_Double (Value);
   end Set_Double;

   procedure Set_Int (Object : Uniform; Value : Integer_32) is
   begin
      Object.GL_Uniform.Set_Int (Value);
   end Set_Int;

   procedure Set_UInt (Object : Uniform; Value : Unsigned_32) is
   begin
      Object.GL_Uniform.Set_UInt (Value);
   end Set_UInt;

   procedure Set_Integer (Object : Uniform; Value : Integer) is
   begin
      Object.GL_Uniform.Set_Int (Integer_32 (Value));
   end Set_Integer;

   procedure Set_Boolean (Object : Uniform; Value : Boolean) is
   begin
      Object.GL_Uniform.Set_Int (if Value then 1 else 0);
   end Set_Boolean;

   -----------------------------------------------------------------------------

   function Create_Uniform_Sampler
     (Object : Program;
      Name   : String) return Uniform_Sampler
   is
      Sampler_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
   begin
      return Uniform_Sampler'
        (Kind       => Sampler_Kind,
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
        (Kind       => Image_Kind,
         GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   exception
      when Constraint_Error =>
         raise Uniform_Type_Error with
           "Uniform " & Name & " has unexpected image type " & Image_Kind'Image;
   end Create_Uniform_Image;

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
