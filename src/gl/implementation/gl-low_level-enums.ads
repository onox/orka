--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

package GL.Low_Level.Enums is
   pragma Preelaborate;

   -- Unlike GL.Enums, this package is not private and hosts enum types that may
   -- be needed by third-party code or wrappers.

   type Texture_Kind is (Texture_1D, Texture_2D, Texture_3D,
                         Texture_Rectangle, Texture_Cube_Map,
                         Texture_1D_Array, Texture_2D_Array,
                         Texture_Buffer, Texture_Cube_Map_Array,
                         Texture_2D_Multisample, Texture_2D_Multisample_Array);

   type Renderbuffer_Kind is (Renderbuffer);

   type Framebuffer_Kind is (Read, Draw);

   type Transform_Feedback_Kind is (Transform_Feedback);

   type Buffer_Kind is (Array_Buffer, Element_Array_Buffer, Pixel_Pack_Buffer,
                        Pixel_Unpack_Buffer, Uniform_Buffer, Texture_Buffer,
                        Transform_Feedback_Buffer, Copy_Read_Buffer,
                        Copy_Write_Buffer, Draw_Indirect_Buffer,
                        Shader_Storage_Buffer, Dispatch_Indirect_Buffer,
                        Query_Buffer, Atomic_Counter_Buffer);

   type Draw_Buffer_Index is (DB0, DB1, DB2, DB3, DB4, DB5, DB6, DB7,
      DB8, DB9, DB10, DB11, DB12, DB13, DB14, DB15);

   type Only_Depth_Buffer is (Depth_Buffer);
   type Only_Stencil_Buffer is (Stencil);
   type Only_Depth_Stencil_Buffer is (Depth_Stencil);
   type Only_Color_Buffer is (Color);

   type Resource_Type is
     (Int_Type,
      UInt_Type,
      Single_Type,
      Double_Type,
      Single_Vec2,
      Single_Vec3,
      Single_Vec4,
      Int_Vec2,
      Int_Vec3,
      Int_Vec4,
      Bool_Type,
      Bool_Vec2,
      Bool_Vec3,
      Bool_Vec4,
      Single_Matrix2,
      Single_Matrix3,
      Single_Matrix4,
      Sampler_1D,
      Sampler_2D,
      Sampler_3D,
      Sampler_Cube,
      Sampler_1D_Shadow,
      Sampler_2D_Shadow,
      Sampler_2D_Rect,
      Sampler_2D_Rect_Shadow,
      Single_Matrix2x3,
      Single_Matrix2x4,
      Single_Matrix3x2,
      Single_Matrix3x4,
      Single_Matrix4x2,
      Single_Matrix4x3,
      Sampler_1D_Array,
      Sampler_2D_Array,
      Sampler_Buffer,
      Sampler_1D_Array_Shadow,
      Sampler_2D_Array_Shadow,
      Sampler_Cube_Shadow,
      UInt_Vec2,
      UInt_Vec3,
      UInt_Vec4,
      Int_Sampler_1D,
      Int_Sampler_2D,
      Int_Sampler_3D,
      Int_Sampler_Cube,
      Int_Sampler_2D_Rect,
      Int_Sampler_1D_Array,
      Int_Sampler_2D_Array,
      Int_Sampler_Buffer,
      UInt_Sampler_1D,
      UInt_Sampler_2D,
      UInt_Sampler_3D,
      UInt_Sampler_Cube,
      UInt_Sampler_2D_Rect,
      UInt_Sampler_1D_Array,
      UInt_Sampler_2D_Array,
      UInt_Sampler_Buffer,
      Double_Matrix2,
      Double_Matrix3,
      Double_Matrix4,
      Double_Matrix2x3,
      Double_Matrix2x4,
      Double_Matrix3x2,
      Double_Matrix3x4,
      Double_Matrix4x2,
      Double_Matrix4x3,
      Double_Vec2,
      Double_Vec3,
      Double_Vec4,
      Sampler_Cube_Array,
      Sampler_Cube_Array_Shadow,
      Int_Sampler_Cube_Array,
      UInt_Sampler_Cube_Array,

      Image_1D,
      Image_2D,
      Image_3D,
      Image_2D_Rect,
      Image_Cube,
      Image_Buffer,
      Image_1D_Array,
      Image_2D_Array,
      Image_Cube_Map_Array,
      Image_2D_Multisample,
      Image_2D_Multisample_Array,
      Int_Image_1D,
      Int_Image_2D,
      Int_Image_3D,
      Int_Image_2D_Rect,
      Int_Image_Cube,
      Int_Image_Buffer,
      Int_Image_1D_Array,
      Int_Image_2D_Array,
      Int_Image_Cube_Map_Array,
      Int_Image_2D_Multisample,
      Int_Image_2D_Multisample_Array,
      UInt_Image_1D,
      UInt_Image_2D,
      UInt_Image_3D,
      UInt_Image_2D_Rect,
      UInt_Image_Cube,
      UInt_Image_Buffer,
      UInt_Image_1D_Array,
      UInt_Image_2D_Array,
      UInt_Image_Cube_Map_Array,
      UInt_Image_2D_Multisample,
      UInt_Image_2D_Multisample_Array,

      Sampler_2D_Multisample,
      Int_Sampler_2D_Multisample,
      UInt_Sampler_2D_Multisample,
      Sampler_2D_Multisample_Array,
      Int_Sampler_2D_Multisample_Array,
      UInt_Sampler_2D_Multisample_Array);

private

   for Texture_Kind use (Texture_1D                   => 16#0DE0#,
                         Texture_2D                   => 16#0DE1#,
                         Texture_3D                   => 16#806F#,
                         Texture_Rectangle            => 16#84F5#,
                         Texture_Cube_Map             => 16#8513#,
                         Texture_1D_Array             => 16#8C18#,
                         Texture_2D_Array             => 16#8C1A#,
                         Texture_Buffer               => 16#8C2A#,
                         Texture_Cube_Map_Array       => 16#9009#,
                         Texture_2D_Multisample       => 16#9100#,
                         Texture_2D_Multisample_Array => 16#9102#);
   for Texture_Kind'Size use Enum'Size;

   for Renderbuffer_Kind use (Renderbuffer => 16#8D41#);
   for Renderbuffer_Kind'Size use Enum'Size;

   for Framebuffer_Kind use (Read      => 16#8CA8#,
                             Draw      => 16#8CA9#);
   for Framebuffer_Kind'Size use Enum'Size;

   for Transform_Feedback_Kind use (Transform_Feedback => 16#8E22#);
   for Transform_Feedback_Kind'Size use Enum'Size;

   for Buffer_Kind use (Array_Buffer              => 16#8892#,
                        Element_Array_Buffer      => 16#8893#,
                        Pixel_Pack_Buffer         => 16#88EB#,
                        Pixel_Unpack_Buffer       => 16#88EC#,
                        Uniform_Buffer            => 16#8A11#,
                        Texture_Buffer            => 16#8C2A#,
                        Transform_Feedback_Buffer => 16#8C8E#,
                        Copy_Read_Buffer          => 16#8F36#,
                        Copy_Write_Buffer         => 16#8F37#,
                        Draw_Indirect_Buffer      => 16#8F3F#,
                        Shader_Storage_Buffer     => 16#90D2#,
                        Dispatch_Indirect_Buffer  => 16#90EE#,
                        Query_Buffer              => 16#9192#,
                        Atomic_Counter_Buffer     => 16#92C0#);
   for Buffer_Kind'Size use Enum'Size;

   for Draw_Buffer_Index use (DB0  => 16#8825#,
                              DB1  => 16#8826#,
                              DB2  => 16#8827#,
                              DB3  => 16#8828#,
                              DB4  => 16#8829#,
                              DB5  => 16#882A#,
                              DB6  => 16#882B#,
                              DB7  => 16#882C#,
                              DB8  => 16#882D#,
                              DB9  => 16#882E#,
                              DB10 => 16#882F#,
                              DB11 => 16#8830#,
                              DB12 => 16#8831#,
                              DB13 => 16#8832#,
                              DB14 => 16#8833#,
                              DB15 => 16#8834#);
   for Draw_Buffer_Index'Size use Int'Size;

   for Only_Depth_Buffer use (Depth_Buffer => 16#1801#);
   for Only_Depth_Buffer'Size use Enum'Size;

   for Only_Stencil_Buffer use (Stencil => 16#1802#);
   for Only_Stencil_Buffer'Size use Enum'Size;

   for Only_Depth_Stencil_Buffer use (Depth_Stencil => 16#84F9#);
   for Only_Depth_Stencil_Buffer'Size use Enum'Size;

   for Only_Color_Buffer use (Color => 16#1800#);
   for Only_Color_Buffer'Size use Enum'Size;

   for Resource_Type use
     (Int_Type                          => 16#1404#,
      UInt_Type                         => 16#1405#,
      Single_Type                       => 16#1406#,
      Double_Type                       => 16#140A#,
      Single_Vec2                       => 16#8B50#,
      Single_Vec3                       => 16#8B51#,
      Single_Vec4                       => 16#8B52#,
      Int_Vec2                          => 16#8B53#,
      Int_Vec3                          => 16#8B54#,
      Int_Vec4                          => 16#8B55#,
      Bool_Type                         => 16#8B56#,
      Bool_Vec2                         => 16#8B57#,
      Bool_Vec3                         => 16#8B58#,
      Bool_Vec4                         => 16#8B59#,
      Single_Matrix2                    => 16#8B5A#,
      Single_Matrix3                    => 16#8B5B#,
      Single_Matrix4                    => 16#8B5C#,
      Sampler_1D                        => 16#8B5D#,
      Sampler_2D                        => 16#8B5E#,
      Sampler_3D                        => 16#8B5F#,
      Sampler_Cube                      => 16#8B60#,
      Sampler_1D_Shadow                 => 16#8B61#,
      Sampler_2D_Shadow                 => 16#8B62#,
      Sampler_2D_Rect                   => 16#8B63#,
      Sampler_2D_Rect_Shadow            => 16#8B64#,
      Single_Matrix2x3                  => 16#8B65#,
      Single_Matrix2x4                  => 16#8B66#,
      Single_Matrix3x2                  => 16#8B67#,
      Single_Matrix3x4                  => 16#8B68#,
      Single_Matrix4x2                  => 16#8B69#,
      Single_Matrix4x3                  => 16#8B6A#,
      Sampler_1D_Array                  => 16#8DC0#,
      Sampler_2D_Array                  => 16#8DC1#,
      Sampler_Buffer                    => 16#8DC2#,
      Sampler_1D_Array_Shadow           => 16#8DC3#,
      Sampler_2D_Array_Shadow           => 16#8DC4#,
      Sampler_Cube_Shadow               => 16#8DC5#,
      UInt_Vec2                         => 16#8DC6#,
      UInt_Vec3                         => 16#8DC7#,
      UInt_Vec4                         => 16#8DC8#,
      Int_Sampler_1D                    => 16#8DC9#,
      Int_Sampler_2D                    => 16#8DCA#,
      Int_Sampler_3D                    => 16#8DCB#,
      Int_Sampler_Cube                  => 16#8DCC#,
      Int_Sampler_2D_Rect               => 16#8DCD#,
      Int_Sampler_1D_Array              => 16#8DCE#,
      Int_Sampler_2D_Array              => 16#8DCF#,
      Int_Sampler_Buffer                => 16#8DD0#,
      UInt_Sampler_1D                   => 16#8DD1#,
      UInt_Sampler_2D                   => 16#8DD2#,
      UInt_Sampler_3D                   => 16#8DD3#,
      UInt_Sampler_Cube                 => 16#8DD4#,
      UInt_Sampler_2D_Rect              => 16#8DD5#,
      UInt_Sampler_1D_Array             => 16#8DD6#,
      UInt_Sampler_2D_Array             => 16#8DD7#,
      UInt_Sampler_Buffer               => 16#8DD8#,
      Double_Matrix2                    => 16#8F46#,
      Double_Matrix3                    => 16#8F47#,
      Double_Matrix4                    => 16#8F48#,
      Double_Matrix2x3                  => 16#8F49#,
      Double_Matrix2x4                  => 16#8F4A#,
      Double_Matrix3x2                  => 16#8F4B#,
      Double_Matrix3x4                  => 16#8F4C#,
      Double_Matrix4x2                  => 16#8F4D#,
      Double_Matrix4x3                  => 16#8F4E#,
      Double_Vec2                       => 16#8FFC#,
      Double_Vec3                       => 16#8FFD#,
      Double_Vec4                       => 16#8FFE#,
      Sampler_Cube_Array                => 16#900C#,
      Sampler_Cube_Array_Shadow         => 16#900D#,
      Int_Sampler_Cube_Array            => 16#900E#,
      UInt_Sampler_Cube_Array           => 16#900F#,

      Image_1D                          => 16#904C#,
      Image_2D                          => 16#904D#,
      Image_3D                          => 16#904E#,
      Image_2D_Rect                     => 16#904F#,
      Image_Cube                        => 16#9050#,
      Image_Buffer                      => 16#9051#,
      Image_1D_Array                    => 16#9052#,
      Image_2D_Array                    => 16#9053#,
      Image_Cube_Map_Array              => 16#9054#,
      Image_2D_Multisample              => 16#9055#,
      Image_2D_Multisample_Array        => 16#9056#,
      Int_Image_1D                      => 16#9057#,
      Int_Image_2D                      => 16#9058#,
      Int_Image_3D                      => 16#9059#,
      Int_Image_2D_Rect                 => 16#905A#,
      Int_Image_Cube                    => 16#905B#,
      Int_Image_Buffer                  => 16#905C#,
      Int_Image_1D_Array                => 16#905D#,
      Int_Image_2D_Array                => 16#905E#,
      Int_Image_Cube_Map_Array          => 16#905F#,
      Int_Image_2D_Multisample          => 16#9060#,
      Int_Image_2D_Multisample_Array    => 16#9061#,
      UInt_Image_1D                     => 16#9062#,
      UInt_Image_2D                     => 16#9063#,
      UInt_Image_3D                     => 16#9064#,
      UInt_Image_2D_Rect                => 16#9065#,
      UInt_Image_Cube                   => 16#9066#,
      UInt_Image_Buffer                 => 16#9067#,
      UInt_Image_1D_Array               => 16#9068#,
      UInt_Image_2D_Array               => 16#9069#,
      UInt_Image_Cube_Map_Array         => 16#906A#,
      UInt_Image_2D_Multisample         => 16#906B#,
      UInt_Image_2D_Multisample_Array   => 16#906C#,

      Sampler_2D_Multisample            => 16#9108#,
      Int_Sampler_2D_Multisample        => 16#9109#,
      UInt_Sampler_2D_Multisample       => 16#910A#,
      Sampler_2D_Multisample_Array      => 16#910B#,
      Int_Sampler_2D_Multisample_Array  => 16#910C#,
      UInt_Sampler_2D_Multisample_Array => 16#910D#);
   for Resource_Type'Size use Enum'Size;

end GL.Low_Level.Enums;
