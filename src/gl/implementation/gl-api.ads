--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Runtime_Loading;

with GL.Blending;
with GL.Buffers;
with GL.Debug;
with GL.Debug_Types;
with GL.Enums.Getter;
with GL.Enums.Internalformat;
with GL.Enums.Textures;
with GL.Errors;
with GL.Fences;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Queries;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Pixels.Queries;
with GL.Rasterization;
with GL.Toggles;
with GL.Types.Colors;
with GL.Types.Compute;
with GL.Types.Debug;
with GL.Types.Pointers;
with GL.Viewports;

with Interfaces.C.Strings;

with System;

private package GL.API is
   pragma Preelaborate;

   use GL.Types;

   function GL_Subprogram_Reference (Function_Name : String) return System.Address;
   --  Implementation is platform-specific. Therefore, gl-api.adb is in the
   --  platform-specific source folders.

   package Loader is new Runtime_Loading (GL_Subprogram_Reference);

   --  Everything newer than OpenGL 1.1 will not be statically bound,
   --  but loaded with GL.Low_Level.Loader at runtime.
   --
   --  Also, all functions that have been deprecated with OpenGL 3.0
   --  will not be statically bound, as they may be omitted by implementors
   --  when they choose to only implement the OpenGL Core Profile.

   subtype Zero is Int range 0 .. 0;

   package Get_Error is new Loader.Function_Without_Params
     ("glGetError", Errors.Error_Code);

   package Flush is new Loader.Procedure_Without_Params ("glFlush");

   -----------------------------------------------------------------------------
   --                            Parameter getters                            --
   -----------------------------------------------------------------------------

   package Get_Boolean is new Loader.Getter_With_2_Params
     ("glGetBooleanv", Enums.Getter.Parameter, Low_Level.Bool);

   package Get_Double_Vec2_I is new Loader.Getter_With_3_Params
     ("glGetDoublei_v", Enums.Getter.Parameter, UInt, Doubles.Vector2);

   package Get_Single is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Single);

   package Get_Single_Vec2 is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Singles.Vector2);

   package Get_Single_Vec4_I is new Loader.Getter_With_3_Params
     ("glGetFloati_v", Enums.Getter.Parameter, UInt, Singles.Vector4);

   package Get_Color is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Colors.Color);

   package Get_Enabled_Color is new Loader.Getter_With_3_Params
     ("glGetBooleani_v", Enums.Getter.Parameter, Buffers.Draw_Buffer_Index,
      Colors.Enabled_Color);

   package Get_Long is new Loader.Getter_With_2_Params
     ("glGetInteger64v", Enums.Getter.Parameter, Long);

   package Get_Integer is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Int);

   package Get_Unsigned_Integer is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, UInt);

   package Get_Size is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Size);

   package Get_Size_I is new Loader.Getter_With_3_Params
     ("glGetIntegeri_v", Enums.Getter.Parameter, UInt, Size);

   package Get_Int_Vec4_I is new Loader.Getter_With_3_Params
     ("glGetIntegeri_v", Enums.Getter.Parameter, UInt, Ints.Vector4);

   package Get_Blend_Factor is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Blending.Blend_Factor);

   package Get_Alignment is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Pixels.Alignment);

   package Get_Blend_Equation is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Blending.Equation);

   package Get_Clip_Origin is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Viewports.Viewport_Origin);

   package Get_Clip_Depth_Mode is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Viewports.Depth_Mode);

   package Get_Compare_Function is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Compare_Function);

   package Get_Orientation is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Rasterization.Orientation);

   package Get_Face_Selector is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Rasterization.Face_Selector);

   package Get_Polygon_Mode is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Rasterization.Polygon_Mode_Type);

   package Get_Logic_Op is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Blending.Logic_Op);

   package Get_Stencil_Action is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Buffers.Stencil_Action);

   package Get_String is new Loader.Function_With_1_Param
     ("glGetString", Enums.Getter.String_Parameter, C.Strings.chars_ptr);

   package Get_String_I is new Loader.Function_With_2_Params
     ("glGetStringi", Enums.Getter.String_Parameter, UInt, C.Strings.chars_ptr);

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   package Enable is new Loader.Procedure_With_1_Param
     ("glEnable", Toggles.Toggle);

   package Disable is new Loader.Procedure_With_1_Param
     ("glDisable", Toggles.Toggle);

   package Is_Enabled is new Loader.Function_With_1_Param
     ("glIsEnabled", Toggles.Toggle, Low_Level.Bool);

   package Enable_I is new Loader.Procedure_With_2_Params
     ("glEnablei", Toggles.Toggle_Indexed, UInt);

   package Disable_I is new Loader.Procedure_With_2_Params
     ("glDisablei", Toggles.Toggle_Indexed, UInt);

   package Is_Enabled_I is new Loader.Function_With_2_Params
     ("glIsEnabledi", Toggles.Toggle_Indexed, UInt, Low_Level.Bool);

   -----------------------------------------------------------------------------
   --                                 Clipping                                --
   -----------------------------------------------------------------------------

   package Clip_Control is new Loader.Procedure_With_2_Params
     ("glClipControl", Viewports.Viewport_Origin, Viewports.Depth_Mode);

   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------

   package Cull_Face is new Loader.Procedure_With_1_Param
     ("glCullFace", Rasterization.Face_Selector);

   package Front_Face is new Loader.Procedure_With_1_Param
     ("glFrontFace", Rasterization.Orientation);

   -----------------------------------------------------------------------------
   --                               Pixel stuff                               --
   -----------------------------------------------------------------------------

   package Pixel_Store_Size is new Loader.Procedure_With_2_Params
     ("glPixelStorei", Enums.Pixel_Store_Param, Size);

   package Pixel_Store_Alignment is new Loader.Procedure_With_2_Params
     ("glPixelStorei", Enums.Pixel_Store_Param, Pixels.Alignment);

   -----------------------------------------------------------------------------
   --                                 Drawing                                 --
   -----------------------------------------------------------------------------

   package Draw_Arrays_Instanced_Base_Instance is new Loader.Procedure_With_5_Params
     ("glDrawArraysInstancedBaseInstance", Connection_Mode, Int, Size, Size, UInt);

   package Multi_Draw_Arrays_Indirect is new Loader.Procedure_With_4_Params
     ("glMultiDrawArraysIndirect", Connection_Mode, Int, Size, Size);

   package Multi_Draw_Arrays_Indirect_Count is new Loader.Procedure_With_5_Params
     ("glMultiDrawArraysIndirectCount", Connection_Mode, Int, Low_Level.IntPtr, Size, Size);

   package Draw_Elements_Instanced_Base_Vertex_Base_Instance
     is new Loader.Procedure_With_7_Params
     ("glDrawElementsInstancedBaseVertexBaseInstance", Connection_Mode, Size,
      Index_Type, Low_Level.IntPtr, Size, Int, UInt);

   package Multi_Draw_Elements_Indirect is new Loader.Procedure_With_5_Params
     ("glMultiDrawElementsIndirect", Connection_Mode, Index_Type,
      Int, Size, Size);

   package Multi_Draw_Elements_Indirect_Count is new Loader.Procedure_With_6_Params
     ("glMultiDrawElementsIndirectCount", Connection_Mode, Index_Type,
      Int, Low_Level.IntPtr, Size, Size);

   -----------------------------------------------------------------------------
   --                                Blending                                 --
   -----------------------------------------------------------------------------

   package Blend_Func_Separate is new Loader.Procedure_With_4_Params
     ("glBlendFuncSeparate", Blending.Blend_Factor, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor);

   package Blend_Func_Separate_I is new Loader.Procedure_With_5_Params
     ("glBlendFuncSeparatei", Buffers.Draw_Buffer_Index, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor, Blending.Blend_Factor);

   package Blend_Color is new Loader.Procedure_With_4_Params
     ("glBlendColor", Colors.Component, Colors.Component, Colors.Component, 
      Colors.Component);

   package Blend_Equation_Separate is new Loader.Procedure_With_2_Params
     ("glBlendEquationSeparate", Blending.Equation, Blending.Equation);

   package Blend_Equation_Separate_I is new Loader.Procedure_With_3_Params
     ("glBlendEquationSeparatei", Buffers.Draw_Buffer_Index, Blending.Equation,
      Blending.Equation);

   -----------------------------------------------------------------------------
   --                              Rasterization                              --
   -----------------------------------------------------------------------------

   package Polygon_Mode is new Loader.Procedure_With_2_Params
     ("glPolygonMode", Rasterization.Face_Selector, Rasterization.Polygon_Mode_Type);

   package Polygon_Offset_Clamp is new Loader.Procedure_With_3_Params
     ("glPolygonOffsetClamp", Single, Single, Single);

   --  TODO glSampleCoverage, glSampleMaski, glGetMultisample, glGetGraphicsResetStatus

   -----------------------------------------------------------------------------
   --                           Multisample shading                           --
   -----------------------------------------------------------------------------

   package Min_Sample_Shading is new Loader.Procedure_With_1_Param
     ("glMinSampleShading", Single);

   -----------------------------------------------------------------------------
   --                                 Buffers                                 --
   -----------------------------------------------------------------------------

   package Color_Mask is new Loader.Procedure_With_4_Params
     ("glColorMask", Low_Level.Bool, Low_Level.Bool, Low_Level.Bool, Low_Level.Bool);

   package Color_Mask_Indexed is new Loader.Procedure_With_5_Params
     ("glColorMaski", Buffers.Draw_Buffer_Index,
      Low_Level.Bool, Low_Level.Bool, Low_Level.Bool, Low_Level.Bool);

   -----------------------------------------------------------------------------
   --                        Depth and stencil buffers                        --
   -----------------------------------------------------------------------------

   package Depth_Mask is new Loader.Procedure_With_1_Param
     ("glDepthMask", Low_Level.Bool);

   package Depth_Func is new Loader.Procedure_With_1_Param
     ("glDepthFunc", Compare_Function);

   package Stencil_Func_Separate is new Loader.Procedure_With_4_Params
     ("glStencilFuncSeparate", Rasterization.Face_Selector,
      Compare_Function, Int, UInt);

   package Stencil_Op_Separate is new Loader.Procedure_With_4_Params
     ("glStencilOpSeparate", Rasterization.Face_Selector, Buffers.Stencil_Action,
      Buffers.Stencil_Action, Buffers.Stencil_Action);

   package Stencil_Mask_Separate is new Loader.Procedure_With_2_Params
     ("glStencilMaskSeparate", Rasterization.Face_Selector, UInt);

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   package Texture_Parameter_Int is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Int);

   package Get_Texture_Parameter_Int is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Int);

   package Get_Texture_Level_Parameter is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Size);

   package Get_Texture_Level_Parameter_Format_I is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Pixels.Internal_Format);

   package Get_Texture_Level_Parameter_Format_C is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Pixels.Compressed_Format);

   package Delete_Textures is new Loader.Procedure_With_2_Params
     ("glDeleteTextures", Size, Low_Level.UInt_Array);

   package Texture_Buffer is new Loader.Procedure_With_3_Params
     ("glTextureBuffer", UInt, Pixels.Internal_Format_Buffer_Texture, UInt);

   package Texture_Buffer_Range is new Loader.Procedure_With_5_Params
     ("glTextureBufferRange", UInt, Pixels.Internal_Format_Buffer_Texture, UInt,
      Low_Level.IntPtr, Size);

   package Texture_Storage_1D is new Loader.Procedure_With_4_Params
     ("glTextureStorage1D", UInt, Size, Pixels.Internal_Format, Size);

   package Texture_Storage_2D_I is new Loader.Procedure_With_5_Params
     ("glTextureStorage2D", UInt, Size, Pixels.Internal_Format, Size, Size);

   package Texture_Storage_3D_I is new Loader.Procedure_With_6_Params
     ("glTextureStorage3D", UInt, Size, Pixels.Internal_Format, Size, Size, Size);

   package Texture_Storage_2D_C is new Loader.Procedure_With_5_Params
     ("glTextureStorage2D", UInt, Size, Pixels.Compressed_Format, Size, Size);

   package Texture_Storage_3D_C is new Loader.Procedure_With_6_Params
     ("glTextureStorage3D", UInt, Size, Pixels.Compressed_Format, Size, Size, Size);

   package Texture_Storage_2D_Multisample_I is new Loader.Procedure_With_6_Params
     ("glTextureStorage2DMultisample", UInt, Size, Pixels.Internal_Format,
      Size, Size, Low_Level.Bool);

   package Texture_Storage_3D_Multisample_I is new Loader.Procedure_With_7_Params
     ("glTextureStorage3DMultisample", UInt, Size, Pixels.Internal_Format,
      Size, Size, Size, Low_Level.Bool);

   package Texture_Storage_2D_Multisample_C is new Loader.Procedure_With_6_Params
     ("glTextureStorage2DMultisample", UInt, Size, Pixels.Compressed_Format,
      Size, Size, Low_Level.Bool);

   package Texture_Storage_3D_Multisample_C is new Loader.Procedure_With_7_Params
     ("glTextureStorage3DMultisample", UInt, Size, Pixels.Compressed_Format,
      Size, Size, Size, Low_Level.Bool);

   package Texture_Sub_Image_1D is new Loader.Procedure_With_7_Params
     ("glTextureSubImage1D", UInt, Objects.Textures.Mipmap_Level,
      Int, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   package Texture_Sub_Image_2D is new Loader.Procedure_With_9_Params
     ("glTextureSubImage2D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   package Texture_Sub_Image_3D is new Loader.Procedure_With_11_Params
     ("glTextureSubImage3D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   package Compressed_Texture_Sub_Image_2D is new Loader.Procedure_With_9_Params
     ("glCompressedTextureSubImage2D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Size, Size, Pixels.Compressed_Format, Size,
      System.Address);

   package Compressed_Texture_Sub_Image_3D is new Loader.Procedure_With_11_Params
     ("glCompressedTextureSubImage3D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Compressed_Format, Size,
      System.Address);

   package Copy_Image_Sub_Data is new Loader.Procedure_With_15_Params
     ("glCopyImageSubData", UInt, Low_Level.Enums.Texture_Kind,
      Objects.Textures.Mipmap_Level, Int, Int, Int,
      UInt, Low_Level.Enums.Texture_Kind,
      Objects.Textures.Mipmap_Level, Int, Int, Int,
      Size, Size, Size);

   package Clear_Tex_Image is new Loader.Procedure_With_5_Params
     ("glClearTexImage", UInt, Objects.Textures.Mipmap_Level,
      Pixels.Format, Pixels.Data_Type, System.Address);

   package Clear_Tex_Sub_Image is new Loader.Procedure_With_11_Params
     ("glClearTexSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   package Generate_Texture_Mipmap is new Loader.Procedure_With_1_Param
     ("glGenerateTextureMipmap", UInt);

   --  glGetTextureSubImage uses an access value of Interfaces.C.Pointers.Element_Array,
   --  therefore declared in GL.Objects.Textures

   package Get_Compressed_Texture_Sub_Image is new Loader.Procedure_With_10_Params
     ("glGetCompressedTextureSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Size, Types.Pointers.UByte_Array_Access);

   package Invalidate_Tex_Image is new Loader.Procedure_With_2_Params
     ("glInvalidateTexImage", UInt, Objects.Textures.Mipmap_Level);

   package Invalidate_Tex_Sub_Image is new Loader.Procedure_With_8_Params
     ("glInvalidateTexSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size);

   package Create_Textures is new Loader.Getter_With_3_Params
     ("glCreateTextures", Low_Level.Enums.Texture_Kind, Size, UInt);

   package Gen_Textures is new Loader.Getter_With_2_Params
     ("glGenTextures", Size, UInt);
   --  Only to be used by Texture_View

   package Texture_View_I is new Loader.Procedure_With_8_Params
     ("glTextureView", UInt, Low_Level.Enums.Texture_Kind, UInt,
      Pixels.Internal_Format, UInt, UInt, UInt, UInt);

   package Texture_View_C is new Loader.Procedure_With_8_Params
     ("glTextureView", UInt, Low_Level.Enums.Texture_Kind, UInt,
      Pixels.Compressed_Format, UInt, UInt, UInt, UInt);

   package Bind_Textures is new Loader.Procedure_With_3_Params
     ("glBindTextures", Objects.Textures.Texture_Unit, Size, Low_Level.UInt_Array);

   package Get_Internal_Format_A is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Size_Array);

   package Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Size);

   package Get_Internal_Format_Long is new Loader.Getter_With_5_Params
     ("glGetInternalformati64v", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Long_Size);

   package Get_Internal_Format_C is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Compressed_Format,
      Enums.Internalformat.Parameter, Size, Size);

   package Get_Internal_Format_Support is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Pixels.Queries.Support);

   -----------------------------------------------------------------------------
   --                                 Images                                  --
   -----------------------------------------------------------------------------

   package Bind_Image_Textures is new Loader.Procedure_With_3_Params
     ("glBindImageTextures", Objects.Textures.Image_Unit, Size, Low_Level.UInt_Array);

   -----------------------------------------------------------------------------
   --                             Buffer objects                              --
   -----------------------------------------------------------------------------

   package Create_Buffers is new Loader.Getter_With_2_Params
      ("glCreateBuffers", Size, UInt);

   package Delete_Buffers is new Loader.Array_Proc_With_2_Params
      ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);

   package Bind_Buffer is new Loader.Procedure_With_2_Params
      ("glBindBuffer", Enums.Buffer_Kind, UInt);

   package Bind_Buffer_Base is new Loader.Procedure_With_3_Params
      ("glBindBufferBase", Enums.Buffer_Kind, UInt, UInt);

   package Bind_Buffer_Range is new Loader.Procedure_With_5_Params
      ("glBindBufferRange", Enums.Buffer_Kind, UInt, UInt,
       Low_Level.IntPtr, Low_Level.SizeIPtr);

   package Named_Buffer_Sub_Data is new Loader.Procedure_With_4_Params
      ("glNamedBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr,
       System.Address);

   package Named_Buffer_Storage is new Loader.Procedure_With_4_Params
      ("glNamedBufferStorage", UInt, Low_Level.SizeIPtr,
       System.Address, Low_Level.Bitfield);

   --  glMapNamedBufferRange returns an instance of generic Interfaces.C.Pointers.Pointer,
   --  therefore declared in GL.Objects.Buffers

   package Unmap_Named_Buffer is new Loader.Procedure_With_1_Param
     ("glUnmapNamedBuffer", UInt);

   package Invalidate_Buffer_Data is new Loader.Procedure_With_1_Param
     ("glInvalidateBufferData", UInt);

   package Invalidate_Buffer_Sub_Data is new Loader.Procedure_With_3_Params
     ("glInvalidateBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

   package Flush_Mapped_Named_Buffer_Range is new Loader.Procedure_With_3_Params
     ("glFlushMappedNamedBufferRange", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

   package Clear_Named_Buffer_Sub_Data is new Loader.Procedure_With_7_Params
     ("glClearNamedBufferSubData", UInt, Pixels.Internal_Format_Buffer_Texture,
      Low_Level.IntPtr, Low_Level.SizeIPtr, Pixels.Format, Pixels.Data_Type,
      System.Address);

   package Copy_Named_Buffer_Sub_Data is new Loader.Procedure_With_5_Params
     ("glCopyNamedBufferSubData", UInt, UInt, Low_Level.IntPtr, Low_Level.IntPtr,
      Low_Level.SizeIPtr);

   -----------------------------------------------------------------------------
   --                           Vertex Array Objects                          --
   -----------------------------------------------------------------------------

   package Create_Vertex_Arrays is new Loader.Getter_With_2_Params
     ("glCreateVertexArrays", Size, UInt);

   package Delete_Vertex_Arrays is new Loader.Array_Proc_With_2_Params
     ("glDeleteVertexArrays", Size, UInt, Low_Level.UInt_Array);

   package Bind_Vertex_Array is new Loader.Procedure_With_1_Param
     ("glBindVertexArray", UInt);

   -----------------------------------------------------------------------------
   --                    Framebuffer objects and handling                     --
   -----------------------------------------------------------------------------

   package Logic_Op is new Loader.Procedure_With_1_Param
     ("glLogicOp", Blending.Logic_Op);

   package Create_Framebuffers is new Loader.Getter_With_2_Params
     ("glCreateFramebuffers", Size, UInt);

   package Delete_Framebuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteFramebuffers", Size, UInt, Low_Level.UInt_Array);

   package Bind_Framebuffer is new Loader.Procedure_With_2_Params
     ("glBindFramebuffer", Enums.Framebuffer_Kind, UInt);

   package Named_Framebuffer_Draw_Buffers is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferDrawBuffers", UInt, Size, Buffers.Color_Buffer_List);

   package Named_Framebuffer_Read_Buffer is new Loader.Procedure_With_2_Params
     ("glNamedFramebufferReadBuffer", UInt, Buffers.Color_Buffer_Selector);

   package Check_Named_Framebuffer_Status is new Loader.Function_With_2_Params
     ("glCheckNamedFramebufferStatus", UInt, Enums.Framebuffer_Kind,
      Objects.Framebuffers.Framebuffer_Status);

   package Named_Framebuffer_Texture is new Loader.Procedure_With_4_Params
     ("glNamedFramebufferTexture", UInt, Objects.Framebuffers.Attachment_Point,
      UInt, Objects.Textures.Mipmap_Level);

   package Named_Framebuffer_Texture_Layer is new Loader.Procedure_With_5_Params
     ("glNamedFramebufferTextureLayer", UInt, Objects.Framebuffers.Attachment_Point,
      UInt, Objects.Textures.Mipmap_Level, Int);

   package Blit_Named_Framebuffer is new Loader.Procedure_With_12_Params
     ("glBlitNamedFramebuffer", UInt, UInt, Int, Int, Int, Int, Int, Int, Int, Int,
      Low_Level.Bitfield, Objects.Textures.Magnifying_Function);

   package Invalidate_Named_Framebuffer_Data is new Loader.Array_Proc_With_3_Params
     ("glInvalidateNamedFramebufferData", UInt, Size,
      Objects.Framebuffers.Attachment_Point,
      Objects.Framebuffers.Attachment_List);

   package Invalidate_Named_Framebuffer_Data_Default is new Loader.Array_Proc_With_3_Params
     ("glInvalidateNamedFramebufferData", UInt, Size,
      Objects.Framebuffers.Default_Attachment_Point,
      Objects.Framebuffers.Default_Attachment_List);

   package Invalidate_Named_Framebuffer_Sub_Data is new Loader.Procedure_With_7_Params
     ("glInvalidateNamedFramebufferSubData", UInt, Size,
      Objects.Framebuffers.Attachment_List, Int, Int, Size, Size);

   package Invalidate_Named_Framebuffer_Sub_Data_Default is new Loader.Procedure_With_7_Params
     ("glInvalidateNamedFramebufferSubData", UInt, Size,
      Objects.Framebuffers.Default_Attachment_List, Int, Int, Size, Size);

   package Named_Framebuffer_Parameter_Size is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferParameteri", UInt, Enums.Framebuffer_Param, Size);

   package Named_Framebuffer_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferParameteri", UInt, Enums.Framebuffer_Param, Low_Level.Bool);

   package Get_Named_Framebuffer_Parameter_Size is new Loader.Procedure_With_3_Params
     ("glGetNamedFramebufferParameteriv", UInt, Enums.Framebuffer_Param, Low_Level.Size_Access);

   package Get_Named_Framebuffer_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glGetNamedFramebufferParameteriv", UInt, Enums.Framebuffer_Param, Low_Level.Bool_Access);

   package Clear_Named_Framebuffer_Color_Real is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   package Clear_Named_Framebuffer_Color_Signed_Int is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferiv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   package Clear_Named_Framebuffer_Color_Unsigned_Int is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferuiv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   package Clear_Named_Framebuffer_Depth is new Loader.Getter_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Enums.Only_Depth_Buffer,
      Zero, Buffers.Depth);

   package Clear_Named_Framebuffer_Stencil is new Loader.Getter_With_4_Params
     ("glClearNamedFramebufferiv", UInt, Enums.Only_Stencil_Buffer,
      Zero, Buffers.Stencil_Index);

   package Clear_Named_Framebuffer_Depth_Stencil is new Loader.Procedure_With_5_Params
     ("glClearNamedFramebufferfi", UInt, Enums.Only_Depth_Stencil_Buffer,
      Zero, Buffers.Depth, Buffers.Stencil_Index);

   -----------------------------------------------------------------------------
   --                                 Shaders                                 --
   -----------------------------------------------------------------------------

   package Get_Shader_Param is new Loader.Getter_With_3_Params
     ("glGetShaderiv", UInt, Enums.Shader_Param, Int);

   package Create_Shader is new Loader.Function_With_1_Param
     ("glCreateShader", Objects.Shaders.Shader_Type, UInt);

   package Delete_Shader is new Loader.Procedure_With_1_Param
     ("glDeleteShader", UInt);

   package Shader_Source is new Loader.Procedure_With_4_Params
     ("glShaderSource", UInt, Size, Low_Level.CharPtr_Array,
      Low_Level.Int_Array);

   package Get_Shader_Source is new Loader.String_Getter_With_4_Params
     ("glGetShaderSource", Size, UInt);

   package Compile_Shader is new Loader.Procedure_With_1_Param
     ("glCompileShader", UInt);

   package Get_Shader_Info_Log is new Loader.String_Getter_With_4_Params
     ("glGetShaderInfoLog", Size, UInt);

   package Create_Program is new Loader.Function_Without_Params
     ("glCreateProgram", UInt);

   package Delete_Program is new Loader.Procedure_With_1_Param
     ("glDeleteProgram", UInt);

   package Get_Program_Param is new Loader.Getter_With_3_Params
     ("glGetProgramiv", UInt, Enums.Program_Param, Int);

   package Get_Program_Param_Compute is new Loader.Getter_With_3_Params
     ("glGetProgramiv", UInt, Enums.Program_Param, Compute.Dimension_Size_Array);

   package Program_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glProgramParameteri", UInt, Enums.Program_Set_Param, Low_Level.Bool);

   package Attach_Shader is new Loader.Procedure_With_2_Params
     ("glAttachShader", UInt, UInt);

   package Detach_Shader is new Loader.Procedure_With_2_Params
     ("glDetachShader", UInt, UInt);

   package Link_Program is new Loader.Procedure_With_1_Param
     ("glLinkProgram", UInt);

   package Get_Program_Info_Log is new Loader.String_Getter_With_4_Params
     ("glGetProgramInfoLog", Size, UInt);

   package Uniform_Subroutines is new Loader.Procedure_With_3_Params
     ("glUniformSubroutinesuiv", Objects.Shaders.Shader_Type, Size, UInt_Array);

   package Use_Program is new Loader.Procedure_With_1_Param
     ("glUseProgram", UInt);

   package Validate_Program is new Loader.Procedure_With_1_Param
     ("glValidateProgram", UInt);

   package Get_Uniform_Location is new Loader.Function_With_2_Params
     ("glGetUniformLocation", UInt, C.char_array, Int);

   -----------------------------------------------------------------------------
   --                                 Compute                                 --
   -----------------------------------------------------------------------------

   package Dispatch_Compute is new Loader.Procedure_With_3_Params
     ("glDispatchCompute", UInt, UInt, UInt);

   package Dispatch_Compute_Indirect is new Loader.Procedure_With_1_Param
     ("glDispatchComputeIndirect", Low_Level.IntPtr);

   -----------------------------------------------------------------------------
   --                    Program interfaces and resources                     --
   -----------------------------------------------------------------------------

   package Get_Program_Interface is new Loader.Getter_With_4_Params
     ("glGetProgramInterfaceiv", UInt, Enums.Program_Interface,
      Enums.Program_Interface_Param, Low_Level.Int_Array);

   package Get_Program_Resource_Index is new Loader.Function_With_3_Params
     ("glGetProgramResourceIndex", UInt, Enums.Program_Interface,
      C.char_array, UInt);

   package Get_Program_Resource is new Loader.Array_Getter_With_8_Params
     ("glGetProgramResourceiv", UInt, Enums.Program_Interface,
      UInt, Size, Enums.Program_Resource_Array, Int, Int_Array);

   package Get_Program_Resource_Location is new Loader.Function_With_3_Params
     ("glGetProgramResourceLocation", UInt, Enums.Program_Interface,
      C.char_array, Int);

   -----------------------------------------------------------------------------
   --                                Pipelines                                --
   -----------------------------------------------------------------------------

   package Use_Program_Stages is new Loader.Procedure_With_3_Params
     ("glUseProgramStages", UInt, Low_Level.Bitfield, UInt);

   package Create_Program_Pipelines is new Loader.Getter_With_2_Params
     ("glCreateProgramPipelines", Size, UInt);

   package Delete_Program_Pipelines is new Loader.Array_Proc_With_2_Params
     ("glDeleteProgramPipelines", Size, UInt, Low_Level.UInt_Array);

   package Bind_Program_Pipeline is new Loader.Procedure_With_1_Param
     ("glBindProgramPipeline", UInt);

   package Get_Program_Pipeline_Param is new Loader.Getter_With_3_Params
     ("glGetProgramPipelineiv", UInt, Enums.Program_Pipeline_Param, Int);

   package Get_Program_Pipeline_Info_Log is new Loader.String_Getter_With_4_Params
     ("glGetProgramPipelineInfoLog", Size, UInt);

   package Validate_Program_Pipeline is new Loader.Procedure_With_1_Param
     ("glValidateProgramPipeline", UInt);

   package Create_Shader_Program is new Loader.Function_With_3_Params
     ("glCreateShaderProgramv", Objects.Shaders.Shader_Type, Size,
      Low_Level.CharPtr_Array, UInt);

   -----------------------------------------------------------------------------
   --                                 Queries                                 --
   -----------------------------------------------------------------------------

   package Create_Queries is new Loader.Getter_With_3_Params
     ("glCreateQueries", Objects.Queries.Query_Type, Size, UInt);

   package Delete_Queries is new Loader.Array_Proc_With_2_Params
     ("glDeleteQueries", Size, UInt, Low_Level.UInt_Array);

   package Begin_Query_Indexed is new Loader.Procedure_With_3_Params
     ("glBeginQueryIndexed", Objects.Queries.Async_Query_Type, UInt, UInt);

   package End_Query_Indexed is new Loader.Procedure_With_2_Params
     ("glEndQueryIndexed", Objects.Queries.Async_Query_Type, UInt);

   package Query_Counter is new Loader.Procedure_With_2_Params
     ("glQueryCounter", UInt, Objects.Queries.Timestamp_Query_Type);

   package Get_Query_Object_UInt is new Loader.Getter_With_3_Params
     ("glGetQueryObjectuiv", UInt, Objects.Queries.Query_Param, UInt);

   package Get_Query_Object_UInt64 is new Loader.Getter_With_3_Params
     ("glGetQueryObjectui64v", UInt, Objects.Queries.Query_Param, UInt64);

   -----------------------------------------------------------------------------
   --                                Samplers                                 --
   -----------------------------------------------------------------------------

   package Create_Samplers is new Loader.Getter_With_2_Params
     ("glCreateSamplers", Size, UInt);

   package Delete_Samplers is new Loader.Array_Proc_With_2_Params
     ("glDeleteSamplers", Size, UInt, Low_Level.UInt_Array);

   package Bind_Sampler is new Loader.Procedure_With_2_Params
     ("glBindSampler", UInt, UInt);

   package Bind_Samplers is new Loader.Array_Proc_With_3_Params
     ("glBindSamplers", UInt, Size, UInt, Low_Level.UInt_Array);

   package Sampler_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Low_Level.Bool);

   package Sampler_Parameter_Float is new Loader.Procedure_With_3_Params
     ("glSamplerParameterf", UInt, Enums.Textures.Parameter, Single);

   package Sampler_Parameter_Floats is new Loader.Procedure_With_3_Params
     ("glSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   package Sampler_Parameter_Minifying_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Minifying_Function);

   package Sampler_Parameter_Magnifying_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Magnifying_Function);

   package Sampler_Parameter_Wrapping_Mode is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);

   package Sampler_Parameter_Compare_Kind is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);

   package Sampler_Parameter_Compare_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Compare_Function);

   package Get_Sampler_Parameter_Bool is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Low_Level.Bool);

   package Get_Sampler_Parameter_Floats is new Loader.Getter_With_3_Params
     ("glGetSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   package Get_Sampler_Parameter_Minifying_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter,
      Objects.Textures.Minifying_Function);

   package Get_Sampler_Parameter_Magnifying_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter,
      Objects.Textures.Magnifying_Function);

   package Get_Sampler_Parameter_Wrapping_Mode is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);

   package Get_Sampler_Parameter_Compare_Kind is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);

   package Get_Sampler_Parameter_Compare_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Compare_Function);

   -----------------------------------------------------------------------------
   --                                Barriers                                 --
   -----------------------------------------------------------------------------

   package Texture_Barrier is new Loader.Procedure_Without_Params
     ("glTextureBarrier");

   package Memory_Barrier is new Loader.Procedure_With_1_Param
     ("glMemoryBarrier", Low_Level.Bitfield);

   package Memory_Barrier_By_Region is new Loader.Procedure_With_1_Param
     ("glMemoryBarrierByRegion", Low_Level.Bitfield);

   -----------------------------------------------------------------------------
   --                                  Debug                                  --
   -----------------------------------------------------------------------------

   package Debug_Message_Control is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Debug.Source, Debug.Message_Type,
      Debug.Severity, Size, UInt_Array, Low_Level.Bool);

   package Debug_Message_Control_Any_Level is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Debug.Source, Debug.Message_Type,
      Low_Level.Enum, Size, UInt_Array, Low_Level.Bool);

   package Debug_Message_Control_Level is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Low_Level.Enum, Low_Level.Enum,
      Debug.Severity, Size, UInt_Array, Low_Level.Bool);

   package Debug_Message_Insert is new Loader.Procedure_With_6_Params
     ("glDebugMessageInsert", Debug.Source, Debug.Message_Type,
      UInt, Debug.Severity, Size, C.char_array);

   package Debug_Message_Callback is new Loader.Procedure_With_2_Params
     ("glDebugMessageCallback", System.Address, System.Address);

   package Get_Debug_Message_Log is new Loader.Function_With_8_Params
     ("glGetDebugMessageLog", UInt, Size, Debug_Types.Source_Array_Access,
      Debug_Types.Type_Array_Access, Debug_Types.UInt_Array_Access,
      Debug_Types.Severity_Array_Access, Debug_Types.Size_Array_Access,
      Debug_Types.String_Access, UInt);

   package Push_Debug_Group is new Loader.Procedure_With_4_Params
     ("glPushDebugGroup", Debug.Source, UInt, Size, C.char_array);

   package Pop_Debug_Group is new Loader.Procedure_Without_Params
    ("glPopDebugGroup");

   package Object_Label is new Loader.Procedure_With_4_Params
     ("glObjectLabel", Types.Debug.Identifier, UInt, Size, C.char_array);

   package Get_Object_Label is new Loader.String_Getter_With_5_Params
     ("glGetObjectLabel", Size, Types.Debug.Identifier, UInt);

   package Get_Object_Label_Length is new Loader.Procedure_With_5_Params
     ("glGetObjectLabel", Types.Debug.Identifier, UInt, Size,
      Low_Level.Size_Access, C.Strings.chars_ptr);

   package Object_Pointer_Label is new Loader.Procedure_With_3_Params
     ("glObjectPtrLabel", System.Address, Size, C.char_array);
   --  TODO Use for Sync objects

   package Get_Object_Pointer_Label is new Loader.String_Getter_With_4_Params
     ("glGetObjectPtrLabel", Size, System.Address);
   --  TODO Use for Sync objects

   -----------------------------------------------------------------------------
   --                                 Syncing                                 --
   -----------------------------------------------------------------------------

   package Fence_Sync is new Loader.Function_With_2_Params
     ("glFenceSync", Low_Level.Enum, Low_Level.Bitfield, Low_Level.Sync);

   package Delete_Sync is new Loader.Procedure_With_1_Param
     ("glDeleteSync", Low_Level.Sync);

   package Get_Sync is new Loader.Array_Getter_With_5_Params
     ("glGetSynciv", Low_Level.Sync, Low_Level.Enum, Int, Int_Array);

   package Client_Wait_Sync is new Loader.Function_With_3_Params
     ("glClientWaitSync", Low_Level.Sync, Low_Level.Bitfield, UInt64,
      Fences.Wait_Status);

   package Wait_Sync is new Loader.Procedure_With_3_Params
     ("glWaitSync", Low_Level.Sync, Low_Level.Bitfield, UInt64);

   -----------------------------------------------------------------------------
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------

   package Depth_Range_Array is new Loader.Procedure_With_3_Params
     ("glDepthRangeArrayv", UInt, Size, Viewports.Depth_Range_List);

   package Viewport_Array is new Loader.Procedure_With_3_Params
     ("glViewportArrayv", UInt, Size, Viewports.Viewport_List);

   package Scissor_Array is new Loader.Procedure_With_3_Params
     ("glScissorArrayv", UInt, Size, Viewports.Scissor_Rectangle_List);

end GL.API;
