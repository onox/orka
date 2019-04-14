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
with GL.Clipping;
with GL.Culling;
with GL.Debug;
with GL.Debug_Types;
with GL.Enums.Getter;
with GL.Enums.Internalformat;
with GL.Enums.Textures;
with GL.Errors;
with GL.Fences;
with GL.Framebuffer;
with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Framebuffers;
with GL.Objects.Programs;
with GL.Objects.Queries;
with GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Objects.Transform_Feedbacks;
with GL.Objects.Vertex_Arrays;
with GL.Pixels.Queries;
with GL.Rasterization;
with GL.Toggles;
with GL.Types.Colors;
with GL.Types.Compute;
with GL.Types.Debug;
with GL.Window;

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

   function Get_Error is new Loader.Function_Without_Params
     ("glGetError", Errors.Error_Code);

   procedure Flush is new Loader.Procedure_Without_Params ("glFlush");

   procedure Finish is new Loader.Procedure_Without_Params ("glFinish");

   -----------------------------------------------------------------------------
   --                            Parameter getters                            --
   -----------------------------------------------------------------------------

   procedure Get_Boolean is new Loader.Getter_With_2_Params
     ("glGetBooleanv", Enums.Getter.Parameter, Low_Level.Bool);

   procedure Get_Double is new Loader.Getter_With_2_Params
     ("glGetDoublev", Enums.Getter.Parameter, Double);

   procedure Get_Double_Vec2_I is new Loader.Getter_With_3_Params
     ("glGetDoublei_v", Enums.Getter.Parameter, UInt, Doubles.Vector2);

   procedure Get_Single is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Single);

   procedure Get_Single_Vec2 is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Singles.Vector2);

   procedure Get_Single_Vec4_I is new Loader.Getter_With_3_Params
     ("glGetFloati_v", Enums.Getter.Parameter, UInt, Singles.Vector4);

   procedure Get_Color is new Loader.Getter_With_2_Params
     ("glGetFloatv", Enums.Getter.Parameter, Colors.Color);

   procedure Get_Enabled_Color is new Loader.Getter_With_3_Params
     ("glGetBooleani_v", Enums.Getter.Parameter, Buffers.Draw_Buffer_Index,
      Colors.Enabled_Color);

   procedure Get_Long is new Loader.Getter_With_2_Params
     ("glGetInteger64v", Enums.Getter.Parameter, Long);

   procedure Get_Integer is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Int);

   procedure Get_Unsigned_Integer is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, UInt);

   procedure Get_Size is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Size);

   procedure Get_Size_I is new Loader.Getter_With_3_Params
     ("glGetIntegeri_v", Enums.Getter.Parameter, UInt, Size);

   procedure Get_Int_Vec4_I is new Loader.Getter_With_3_Params
     ("glGetIntegeri_v", Enums.Getter.Parameter, UInt, Ints.Vector4);

   procedure Get_Blend_Factor is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Blending.Blend_Factor);

   procedure Get_Alignment is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Pixels.Alignment);

   procedure Get_Blend_Equation is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Blending.Equation);

   procedure Get_Clip_Origin is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Clipping.Viewport_Origin);

   procedure Get_Clip_Depth_Mode is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Clipping.Depth_Mode);

   procedure Get_Compare_Function is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Compare_Function);

   procedure Get_Orientation is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Culling.Orientation);

   procedure Get_Face_Selector is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Culling.Face_Selector);

   procedure Get_Polygon_Mode is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Rasterization.Polygon_Mode_Type);

   procedure Get_Logic_Op is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Framebuffer.Logic_Op);

   procedure Get_Stencil_Action is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Buffers.Stencil_Action);

   procedure Get_Read_Buffer_Selector is new Loader.Getter_With_2_Params
     ("glGetIntegerv", Enums.Getter.Parameter, Buffers.Color_Buffer_Selector);

   function Get_String is new Loader.Function_With_1_Param
     ("glGetString", Enums.Getter.String_Parameter, C.Strings.chars_ptr);

   function Get_String_I is new Loader.Function_With_2_Params
     ("glGetStringi", Enums.Getter.String_Parameter, UInt, C.Strings.chars_ptr);

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   procedure Enable is new Loader.Procedure_With_1_Param
     ("glEnable", Toggles.Toggle);

   procedure Disable is new Loader.Procedure_With_1_Param
     ("glDisable", Toggles.Toggle);

   function Is_Enabled is new Loader.Function_With_1_Param
     ("glIsEnabled", Toggles.Toggle, Low_Level.Bool);

   procedure Enable_I is new Loader.Procedure_With_2_Params
     ("glEnablei", Toggles.Toggle_Indexed, UInt);

   procedure Disable_I is new Loader.Procedure_With_2_Params
     ("glDisablei", Toggles.Toggle_Indexed, UInt);

   function Is_Enabled_I is new Loader.Function_With_2_Params
     ("glIsEnabledi", Toggles.Toggle_Indexed, UInt, Low_Level.Bool);

   -----------------------------------------------------------------------------
   --                                 Clipping                                --
   -----------------------------------------------------------------------------

   procedure Clip_Control is new Loader.Procedure_With_2_Params
     ("glClipControl", Clipping.Viewport_Origin, Clipping.Depth_Mode);

   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------

   procedure Cull_Face is new Loader.Procedure_With_1_Param
     ("glCullFace", Culling.Face_Selector);

   procedure Front_Face is new Loader.Procedure_With_1_Param
     ("glFrontFace", Culling.Orientation);

   -----------------------------------------------------------------------------
   --                               Pixel stuff                               --
   -----------------------------------------------------------------------------

   procedure Pixel_Store is new Loader.Procedure_With_2_Params
     ("glPixelStorei", Enums.Pixel_Store_Param, Low_Level.Bool);

   procedure Pixel_Store is new Loader.Procedure_With_2_Params
     ("glPixelStorei", Enums.Pixel_Store_Param, Size);

   procedure Pixel_Store is new Loader.Procedure_With_2_Params
     ("glPixelStorei", Enums.Pixel_Store_Param, Pixels.Alignment);

   -----------------------------------------------------------------------------
   --                                 Drawing                                 --
   -----------------------------------------------------------------------------

   procedure Draw_Arrays is new Loader.Procedure_With_3_Params
     ("glDrawArrays", Connection_Mode, Int, Size);

   procedure Draw_Arrays_Instanced_Base_Instance is new Loader.Procedure_With_5_Params
     ("glDrawArraysInstancedBaseInstance", Connection_Mode, Int, Size, Size, UInt);

   procedure Multi_Draw_Arrays_Indirect is new Loader.Procedure_With_4_Params
     ("glMultiDrawArraysIndirect", Connection_Mode, Int, Size, Size);

   procedure Multi_Draw_Arrays_Indirect_Count is new Loader.Procedure_With_5_Params
     ("glMultiDrawArraysIndirectCount", Connection_Mode, Int, Low_Level.IntPtr, Size, Size);

   procedure Draw_Elements is new Loader.Procedure_With_4_Params
     ("glDrawElements", Connection_Mode, Size, Index_Type, Low_Level.IntPtr);

   procedure Draw_Elements_Instanced_Base_Instance is new Loader.Procedure_With_6_Params
     ("glDrawElementsInstancedBaseInstance", Connection_Mode, Size,
      Index_Type, Low_Level.IntPtr, Size, UInt);

   procedure Draw_Elements_Instanced_Base_Vertex_Base_Instance
     is new Loader.Procedure_With_7_Params
     ("glDrawElementsInstancedBaseVertexBaseInstance", Connection_Mode, Size,
      Index_Type, Low_Level.IntPtr, Size, Int, UInt);

   procedure Multi_Draw_Elements_Indirect is new Loader.Procedure_With_5_Params
     ("glMultiDrawElementsIndirect", Connection_Mode, Index_Type,
      Int, Size, Size);

   procedure Multi_Draw_Elements_Indirect_Count is new Loader.Procedure_With_6_Params
     ("glMultiDrawElementsIndirectCount", Connection_Mode, Index_Type,
      Int, Low_Level.IntPtr, Size, Size);

   -----------------------------------------------------------------------------
   --                                Blending                                 --
   -----------------------------------------------------------------------------

   procedure Blend_Func_Separate is new Loader.Procedure_With_4_Params
     ("glBlendFuncSeparate", Blending.Blend_Factor, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor);

   procedure Blend_Func_Separate_I is new Loader.Procedure_With_5_Params
     ("glBlendFuncSeparatei", Buffers.Draw_Buffer_Index, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor, Blending.Blend_Factor);

   procedure Blend_Color is new Loader.Procedure_With_4_Params
     ("glBlendColor", Colors.Component, Colors.Component, Colors.Component, 
      Colors.Component);

   procedure Blend_Equation_Separate is new Loader.Procedure_With_2_Params
     ("glBlendEquationSeparate", Blending.Equation, Blending.Equation);

   procedure Blend_Equation_Separate_I is new Loader.Procedure_With_3_Params
     ("glBlendEquationSeparatei", Buffers.Draw_Buffer_Index, Blending.Equation,
      Blending.Equation);

   -----------------------------------------------------------------------------
   --                              Rasterization                              --
   -----------------------------------------------------------------------------

   procedure Line_Width is new Loader.Procedure_With_1_Param
     ("glLineWidth", Single);

   procedure Polygon_Mode is new Loader.Procedure_With_2_Params
     ("glPolygonMode", Culling.Face_Selector, Rasterization.Polygon_Mode_Type);

   procedure Polygon_Offset_Clamp is new Loader.Procedure_With_3_Params
     ("glPolygonOffsetClamp", Single, Single, Single);

   --  TODO glPointSize, glSampleCoverage, glSampleMaski, glPointParameter{if}[v]
   --  TODO glGetMultisample, glGetGraphicsResetStatus

   -----------------------------------------------------------------------------
   --                           Multisample shading                           --
   -----------------------------------------------------------------------------

   procedure Min_Sample_Shading is new Loader.Procedure_With_1_Param
     ("glMinSampleShading", Single);

   -----------------------------------------------------------------------------
   --                                 Buffers                                 --
   -----------------------------------------------------------------------------

   procedure Color_Mask is new Loader.Procedure_With_4_Params
     ("glColorMask", Low_Level.Bool, Low_Level.Bool, Low_Level.Bool, Low_Level.Bool);

   procedure Color_Mask_Indexed is new Loader.Procedure_With_5_Params
     ("glColorMaski", Buffers.Draw_Buffer_Index,
      Low_Level.Bool, Low_Level.Bool, Low_Level.Bool, Low_Level.Bool);

   -----------------------------------------------------------------------------
   --                        Depth and stencil buffers                        --
   -----------------------------------------------------------------------------

   procedure Depth_Mask is new Loader.Procedure_With_1_Param
     ("glDepthMask", Low_Level.Bool);

   procedure Depth_Func is new Loader.Procedure_With_1_Param
     ("glDepthFunc", Compare_Function);

   procedure Stencil_Func_Separate is new Loader.Procedure_With_4_Params
     ("glStencilFuncSeparate", Culling.Face_Selector,
      Compare_Function, Int, UInt);

   procedure Stencil_Op_Separate is new Loader.Procedure_With_4_Params
     ("glStencilOpSeparate", Culling.Face_Selector, Buffers.Stencil_Action,
      Buffers.Stencil_Action, Buffers.Stencil_Action);

   procedure Stencil_Mask_Separate is new Loader.Procedure_With_2_Params
     ("glStencilMaskSeparate", Culling.Face_Selector, UInt);

   -----------------------------------------------------------------------------
   --                                Textures                                 --
   -----------------------------------------------------------------------------

   procedure Texture_Parameter_Min_Filter is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Minifying_Function);
   procedure Texture_Parameter_Mag_Filter is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Magnifying_Function);
   procedure Texture_Parameter_Wrap_Mode is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);
   procedure Texture_Parameter_Compare_Mode is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);
   procedure Texture_Parameter_Compare_Func is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Compare_Function);

   procedure Texture_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Low_Level.Bool);
   procedure Texture_Parameter_Int is new Loader.Procedure_With_3_Params
     ("glTextureParameteri", UInt, Enums.Textures.Parameter, Int);
   procedure Texture_Parameter_Float is new Loader.Procedure_With_3_Params
     ("glTextureParameterf", UInt, Enums.Textures.Parameter, Single);
   procedure Texture_Parameter_Floats is new Loader.Procedure_With_3_Params
     ("glTextureParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   procedure Get_Texture_Parameter_Min_Filter is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Minifying_Function);
   procedure Get_Texture_Parameter_Mag_Filter is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Magnifying_Function);
   procedure Get_Texture_Parameter_Wrap_Mode is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);
   procedure Get_Texture_Parameter_Compare_Mode is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);
   procedure Get_Texture_Parameter_Compare_Func is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Compare_Function);

   procedure Get_Texture_Parameter_Bool is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Low_Level.Bool);
   procedure Get_Texture_Parameter_Int is new Loader.Getter_With_3_Params
     ("glGetTextureParameteriv", UInt, Enums.Textures.Parameter, Int);
   procedure Get_Texture_Parameter_Floats is new Loader.Getter_With_3_Params
     ("glGetTextureParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   procedure Get_Texture_Level_Parameter_Size is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Size);
   procedure Get_Texture_Level_Parameter_Format is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Pixels.Internal_Format);
   procedure Get_Texture_Level_Parameter_Format is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Pixels.Compressed_Format);
   procedure Get_Texture_Level_Parameter_Type is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Pixels.Channel_Data_Type);
   procedure Get_Texture_Level_Parameter_Bool is new Loader.Getter_With_4_Params
     ("glGetTextureLevelParameteriv", UInt, Objects.Textures.Mipmap_Level,
      Enums.Textures.Level_Parameter, Low_Level.Bool);

   procedure Delete_Textures is new Loader.Procedure_With_2_Params
     ("glDeleteTextures", Size, Low_Level.UInt_Array);

   procedure Texture_Buffer is new Loader.Procedure_With_3_Params
     ("glTextureBuffer", UInt, Pixels.Internal_Format_Buffer_Texture, UInt);

   procedure Texture_Buffer_Range is new Loader.Procedure_With_5_Params
     ("glTextureBufferRange", UInt, Pixels.Internal_Format_Buffer_Texture, UInt,
      Low_Level.IntPtr, Size);

   procedure Texture_Storage_1D is new Loader.Procedure_With_4_Params
     ("glTextureStorage1D", UInt, Size, Pixels.Internal_Format, Size);

   procedure Texture_Storage_2D is new Loader.Procedure_With_5_Params
     ("glTextureStorage2D", UInt, Size, Pixels.Internal_Format, Size, Size);

   procedure Texture_Storage_3D is new Loader.Procedure_With_6_Params
     ("glTextureStorage3D", UInt, Size, Pixels.Internal_Format, Size, Size, Size);

   procedure Texture_Storage_2D is new Loader.Procedure_With_5_Params
     ("glTextureStorage2D", UInt, Size, Pixels.Compressed_Format, Size, Size);

   procedure Texture_Storage_3D is new Loader.Procedure_With_6_Params
     ("glTextureStorage3D", UInt, Size, Pixels.Compressed_Format, Size, Size, Size);

   procedure Texture_Storage_2D_Multisample is new Loader.Procedure_With_6_Params
     ("glTextureStorage2DMultisample", UInt, Size, Pixels.Internal_Format,
      Size, Size, Low_Level.Bool);

   procedure Texture_Storage_3D_Multisample is new Loader.Procedure_With_7_Params
     ("glTextureStorage3DMultisample", UInt, Size, Pixels.Internal_Format,
      Size, Size, Size, Low_Level.Bool);

   procedure Texture_Storage_2D_Multisample is new Loader.Procedure_With_6_Params
     ("glTextureStorage2DMultisample", UInt, Size, Pixels.Compressed_Format,
      Size, Size, Low_Level.Bool);

   procedure Texture_Storage_3D_Multisample is new Loader.Procedure_With_7_Params
     ("glTextureStorage3DMultisample", UInt, Size, Pixels.Compressed_Format,
      Size, Size, Size, Low_Level.Bool);

   procedure Texture_Sub_Image_1D is new Loader.Procedure_With_7_Params
     ("glTextureSubImage1D", UInt, Objects.Textures.Mipmap_Level,
      Int, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   procedure Texture_Sub_Image_2D is new Loader.Procedure_With_9_Params
     ("glTextureSubImage2D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   procedure Texture_Sub_Image_3D is new Loader.Procedure_With_11_Params
     ("glTextureSubImage3D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   procedure Compressed_Texture_Sub_Image_2D is new Loader.Procedure_With_9_Params
     ("glCompressedTextureSubImage2D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Size, Size, Pixels.Compressed_Format, Size,
      System.Address);

   procedure Compressed_Texture_Sub_Image_3D is new Loader.Procedure_With_11_Params
     ("glCompressedTextureSubImage3D", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Compressed_Format, Size,
      System.Address);

   procedure Copy_Image_Sub_Data is new Loader.Procedure_With_15_Params
     ("glCopyImageSubData", UInt, Low_Level.Enums.Texture_Kind,
      Objects.Textures.Mipmap_Level, Int, Int, Int,
      UInt, Low_Level.Enums.Texture_Kind,
      Objects.Textures.Mipmap_Level, Int, Int, Int,
      Size, Size, Size);

   procedure Clear_Tex_Image is new Loader.Procedure_With_5_Params
     ("glClearTexImage", UInt, Objects.Textures.Mipmap_Level,
      Pixels.Format, Pixels.Data_Type, System.Address);

   procedure Clear_Tex_Sub_Image is new Loader.Procedure_With_11_Params
     ("glClearTexSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Pixels.Format, Pixels.Data_Type,
      System.Address);

   procedure Generate_Texture_Mipmap is new Loader.Procedure_With_1_Param
     ("glGenerateTextureMipmap", UInt);

   --  glGetTextureSubImage uses an access value of Interfaces.C.Pointers.Element_Array,
   --  therefore declared in GL.Objects.Textures

   procedure Get_Compressed_Texture_Sub_Image is new Loader.Procedure_With_10_Params
     ("glGetCompressedTextureSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size, Size, UByte_Array_Access);

   procedure Invalidate_Tex_Image is new Loader.Procedure_With_2_Params
     ("glInvalidateTexImage", UInt, Objects.Textures.Mipmap_Level);

   procedure Invalidate_Tex_Sub_Image is new Loader.Procedure_With_8_Params
     ("glInvalidateTexSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size);

   procedure Create_Textures is new Loader.Getter_With_3_Params
     ("glCreateTextures", Low_Level.Enums.Texture_Kind, Size, UInt);

   procedure Bind_Texture_Unit is new Loader.Procedure_With_2_Params
     ("glBindTextureUnit", Objects.Textures.Texture_Unit, UInt);

   procedure Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Size_Array);

   procedure Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Size);

   procedure Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformati64v", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Long_Size);

   procedure Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Compressed_Format,
      Enums.Internalformat.Parameter, Size, Size);

   procedure Get_Internal_Format is new Loader.Getter_With_5_Params
     ("glGetInternalformativ", Low_Level.Enums.Texture_Kind, Pixels.Internal_Format,
      Enums.Internalformat.Parameter, Size, Pixels.Queries.Support);

   -----------------------------------------------------------------------------
   --                                 Images                                  --
   -----------------------------------------------------------------------------

   procedure Bind_Image_Textures is new Loader.Procedure_With_3_Params
     ("glBindImageTextures", Objects.Textures.Image_Unit, Size, Low_Level.UInt_Array);

   -----------------------------------------------------------------------------
   --                             Buffer objects                              --
   -----------------------------------------------------------------------------

   procedure Create_Buffers is new Loader.Getter_With_2_Params
      ("glCreateBuffers", Size, UInt);

   procedure Delete_Buffers is new Loader.Array_Proc_With_2_Params
      ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Buffer is new Loader.Procedure_With_2_Params
      ("glBindBuffer", Enums.Buffer_Kind, UInt);

   procedure Bind_Buffer_Base is new Loader.Procedure_With_3_Params
      ("glBindBufferBase", Enums.Buffer_Kind, UInt, UInt);

   procedure Bind_Buffer_Range is new Loader.Procedure_With_5_Params
      ("glBindBufferRange", Enums.Buffer_Kind, UInt, UInt,
       Low_Level.IntPtr, Low_Level.SizeIPtr);

   procedure Named_Buffer_Sub_Data is new Loader.Procedure_With_4_Params
      ("glNamedBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr,
       System.Address);

   procedure Named_Buffer_Storage is new Loader.Procedure_With_4_Params
      ("glNamedBufferStorage", UInt, Low_Level.SizeIPtr,
       System.Address, Low_Level.Bitfield);

   -- glMapNamedBufferRange returns an instance of generic Interfaces.C.Pointers.Pointer,
   -- therefore declared in GL.Objects.Buffers

   procedure Unmap_Named_Buffer is new Loader.Procedure_With_1_Param
     ("glUnmapNamedBuffer", UInt);

   procedure Get_Named_Buffer_Parameter_Access_Kind is new Loader.Getter_With_3_Params
     ("glGetNamedBufferParameteriv", UInt, Enums.Buffer_Param,
      Objects.Buffers.Access_Kind);

   procedure Get_Named_Buffer_Parameter_Bool is new Loader.Getter_With_3_Params
     ("glGetNamedBufferParameteriv", UInt, Enums.Buffer_Param,
      Low_Level.Bool);

   procedure Get_Named_Buffer_Parameter_Bitfield is new Loader.Getter_With_3_Params
     ("glGetNamedBufferParameteriv", UInt, Enums.Buffer_Param,
      Low_Level.Bitfield);

   procedure Get_Named_Buffer_Parameter_Size is new Loader.Getter_With_3_Params
     ("glGetNamedBufferParameteri64v", UInt, Enums.Buffer_Param,
      Long_Size);

   procedure Invalidate_Buffer_Data is new Loader.Procedure_With_1_Param
     ("glInvalidateBufferData", UInt);

   procedure Invalidate_Buffer_Sub_Data is new Loader.Procedure_With_3_Params
     ("glInvalidateBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

   procedure Flush_Mapped_Named_Buffer_Range is new Loader.Procedure_With_3_Params
     ("glFlushMappedNamedBufferRange", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

   procedure Clear_Named_Buffer_Data is new Loader.Procedure_With_5_Params
     ("glClearNamedBufferData", UInt, Pixels.Internal_Format_Buffer_Texture,
      Pixels.Format, Pixels.Data_Type, System.Address);

   procedure Clear_Named_Buffer_Sub_Data is new Loader.Procedure_With_7_Params
     ("glClearNamedBufferSubData", UInt, Pixels.Internal_Format_Buffer_Texture,
      Low_Level.IntPtr, Low_Level.SizeIPtr, Pixels.Format, Pixels.Data_Type,
      System.Address);

   procedure Copy_Named_Buffer_Sub_Data is new Loader.Procedure_With_5_Params
     ("glCopyNamedBufferSubData", UInt, UInt, Low_Level.IntPtr, Low_Level.IntPtr,
      Low_Level.SizeIPtr);

   -----------------------------------------------------------------------------
   --                           Vertex Array Objects                          --
   -----------------------------------------------------------------------------

   procedure Create_Vertex_Arrays is new Loader.Getter_With_2_Params
     ("glCreateVertexArrays", Size, UInt);

   procedure Delete_Vertex_Arrays is new Loader.Array_Proc_With_2_Params
     ("glDeleteVertexArrays", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Vertex_Array is new Loader.Procedure_With_1_Param
     ("glBindVertexArray", UInt);

   procedure Vertex_Array_Attrib_Format is new Loader.Procedure_With_6_Params
     ("glVertexArrayAttribFormat", UInt, Attribute, Component_Count,
      Numeric_Type, Low_Level.Bool, UInt);

   procedure Vertex_Array_AttribI_Format is new Loader.Procedure_With_5_Params
     ("glVertexArrayAttribIFormat", UInt, Attribute, Component_Count,
      Numeric_Type, UInt);

   procedure Vertex_Array_AttribL_Format is new Loader.Procedure_With_5_Params
     ("glVertexArrayAttribLFormat", UInt, Attribute, Component_Count,
      Numeric_Type, UInt);

   procedure Vertex_Array_Attrib_Binding is new Loader.Procedure_With_3_Params
     ("glVertexArrayAttribBinding", UInt, Attribute, Objects.Vertex_Arrays.Binding);

   procedure Vertex_Array_Binding_Divisor is new Loader.Procedure_With_3_Params
     ("glVertexArrayBindingDivisor", UInt, Objects.Vertex_Arrays.Binding, UInt);

   procedure Vertex_Array_Vertex_Buffer is new Loader.Procedure_With_5_Params
     ("glVertexArrayVertexBuffer", UInt, Objects.Vertex_Arrays.Binding, UInt, Low_Level.IntPtr, Size);

   procedure Vertex_Array_Element_Buffer is new Loader.Procedure_With_2_Params
     ("glVertexArrayElementBuffer", UInt, UInt);

   procedure Enable_Vertex_Array_Attrib is new Loader.Procedure_With_2_Params
     ("glEnableVertexArrayAttrib", UInt, Attribute);

   procedure Disable_Vertex_Array_Attrib is new Loader.Procedure_With_2_Params
     ("glDisableVertexArrayAttrib", UInt, Attribute);

   -----------------------------------------------------------------------------
   --                    Framebuffer objects and handling                     --
   -----------------------------------------------------------------------------

   procedure Logic_Op is new Loader.Procedure_With_1_Param
     ("glLogicOp", Framebuffer.Logic_Op);

   procedure Create_Framebuffers is new Loader.Getter_With_2_Params
     ("glCreateFramebuffers", Size, UInt);

   procedure Delete_Framebuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteFramebuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Framebuffer is new Loader.Procedure_With_2_Params
     ("glBindFramebuffer", Enums.Framebuffer_Kind, UInt);

   procedure Named_Framebuffer_Draw_Buffers is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferDrawBuffers", UInt, Size, Buffers.Color_Buffer_List);

   procedure Named_Framebuffer_Read_Buffer is new Loader.Procedure_With_2_Params
     ("glNamedFramebufferReadBuffer", UInt, Buffers.Color_Buffer_Selector);

   function Check_Named_Framebuffer_Status is new Loader.Function_With_2_Params
     ("glCheckNamedFramebufferStatus", UInt, Enums.Framebuffer_Kind,
      Objects.Framebuffers.Framebuffer_Status);

   procedure Named_Framebuffer_Texture is new Loader.Procedure_With_4_Params
     ("glNamedFramebufferTexture", UInt, Objects.Framebuffers.Attachment_Point,
      UInt, Objects.Textures.Mipmap_Level);

   procedure Named_Framebuffer_Texture_Layer is new Loader.Procedure_With_5_Params
     ("glNamedFramebufferTextureLayer", UInt, Objects.Framebuffers.Attachment_Point,
      UInt, Objects.Textures.Mipmap_Level, Int);

   procedure Blit_Named_Framebuffer is new Loader.Procedure_With_12_Params
     ("glBlitNamedFramebuffer", UInt, UInt, Int, Int, Int, Int, Int, Int, Int, Int,
      Low_Level.Bitfield, Objects.Textures.Magnifying_Function);

   procedure Invalidate_Named_Framebuffer_Data is new Loader.Array_Proc_With_3_Params
     ("glInvalidateNamedFramebufferData", UInt, Size,
      Objects.Framebuffers.Attachment_Point,
      Objects.Framebuffers.Attachment_List);

   procedure Invalidate_Named_Framebuffer_Sub_Data is new Loader.Procedure_With_7_Params
     ("glInvalidateNamedFramebufferSubData", UInt, Size,
      Objects.Framebuffers.Attachment_List, Int, Int, Size, Size);

   procedure Named_Framebuffer_Parameter_Size is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferParameteri", UInt, Enums.Framebuffer_Param, Size);

   procedure Named_Framebuffer_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferParameteri", UInt, Enums.Framebuffer_Param, Low_Level.Bool);

   procedure Get_Named_Framebuffer_Parameter_Size is new Loader.Procedure_With_3_Params
     ("glGetNamedFramebufferParameteriv", UInt, Enums.Framebuffer_Param, Low_Level.Size_Access);

   procedure Get_Named_Framebuffer_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glGetNamedFramebufferParameteriv", UInt, Enums.Framebuffer_Param, Low_Level.Bool_Access);

   procedure Clear_Named_Framebuffer_Color_Real is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   procedure Clear_Named_Framebuffer_Color_Signed_Int is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferiv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   procedure Clear_Named_Framebuffer_Color_Unsigned_Int is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferuiv", UInt, Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   procedure Clear_Named_Framebuffer_Depth is new Loader.Getter_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Enums.Only_Depth_Buffer,
      Zero, Buffers.Depth);

   procedure Clear_Named_Framebuffer_Stencil is new Loader.Getter_With_4_Params
     ("glClearNamedFramebufferiv", UInt, Enums.Only_Stencil_Buffer,
      Zero, Buffers.Stencil_Index);

   procedure Clear_Named_Framebuffer_Depth_Stencil is new Loader.Procedure_With_5_Params
     ("glClearNamedFramebufferfi", UInt, Enums.Only_Depth_Stencil_Buffer,
      Zero, Buffers.Depth, Buffers.Stencil_Index);

   -----------------------------------------------------------------------------
   --                                 Shaders                                 --
   -----------------------------------------------------------------------------

   procedure Get_Shader_Param is new Loader.Getter_With_3_Params
     ("glGetShaderiv", UInt, Enums.Shader_Param, Int);

   procedure Get_Shader_Type is new Loader.Getter_With_3_Params
     ("glGetShaderiv", UInt, Enums.Shader_Param, Objects.Shaders.Shader_Type);

   function Create_Shader is new Loader.Function_With_1_Param
     ("glCreateShader", Objects.Shaders.Shader_Type, UInt);

   procedure Delete_Shader is new Loader.Procedure_With_1_Param
     ("glDeleteShader", UInt);

   procedure Shader_Source is new Loader.Procedure_With_4_Params
     ("glShaderSource", UInt, Size, Low_Level.CharPtr_Array,
      Low_Level.Int_Array);

   procedure Get_Shader_Source is
     new Loader.String_Getter_With_4_Params
     ("glGetShaderSource", Size, UInt);

   procedure Compile_Shader is new Loader.Procedure_With_1_Param
     ("glCompileShader", UInt);

   procedure Release_Shader_Compiler is new Loader.Procedure_Without_Params
     ("glReleaseShaderCompiler");

   procedure Get_Shader_Info_Log is
     new Loader.String_Getter_With_4_Params
     ("glGetShaderInfoLog", Size, UInt);

   function Create_Program is new Loader.Function_Without_Params
     ("glCreateProgram", UInt);

   procedure Delete_Program is new Loader.Procedure_With_1_Param
     ("glDeleteProgram", UInt);

   procedure Get_Program_Param is new Loader.Getter_With_3_Params
     ("glGetProgramiv", UInt, Enums.Program_Param, Int);

   procedure Get_Program_Param is new Loader.Getter_With_3_Params
     ("glGetProgramiv", UInt, Enums.Program_Param, Compute.Dimension_Size_Array);

   procedure Program_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glProgramParameteri", UInt, Enums.Program_Set_Param, Low_Level.Bool);

   procedure Attach_Shader is new Loader.Procedure_With_2_Params
     ("glAttachShader", UInt, UInt);

   procedure Detach_Shader is new Loader.Procedure_With_2_Params
     ("glDetachShader", UInt, UInt);

   procedure Link_Program is new Loader.Procedure_With_1_Param
     ("glLinkProgram", UInt);

   procedure Get_Program_Info_Log is
     new Loader.String_Getter_With_4_Params
     ("glGetProgramInfoLog", Size, UInt);

   procedure Get_Program_Stage is new Loader.Getter_With_4_Params
     ("glGetProgramStageiv", UInt, Objects.Shaders.Shader_Type,
      Enums.Program_Stage_Param, Size);

   procedure Uniform_Subroutines is new Loader.Procedure_With_3_Params
     ("glUniformSubroutinesuiv", Objects.Shaders.Shader_Type, Size, UInt_Array);

   procedure Get_Uniform_Subroutine is new Loader.Getter_With_3_Params
     ("glGetUniformSubroutineuiv", Objects.Shaders.Shader_Type,
      Objects.Programs.Uniform_Location_Type, Objects.Programs.Subroutine_Index_Type);

   procedure Use_Program is new Loader.Procedure_With_1_Param
     ("glUseProgram", UInt);

   procedure Validate_Program is new Loader.Procedure_With_1_Param
     ("glValidateProgram", UInt);

   function Get_Uniform_Location is new Loader.Function_With_2_Params
     ("glGetUniformLocation", UInt, C.char_array, Int);

   function Get_Attached_Shaders is new Loader.Array_Getter_With_4_Params
     ("glGetAttachedShaders", UInt, UInt, UInt_Array);

   -----------------------------------------------------------------------------
   --                                 Compute                                 --
   -----------------------------------------------------------------------------

   procedure Dispatch_Compute is new Loader.Procedure_With_3_Params
     ("glDispatchCompute", UInt, UInt, UInt);

   procedure Dispatch_Compute_Indirect is new Loader.Procedure_With_1_Param
     ("glDispatchComputeIndirect", Low_Level.IntPtr);

   -----------------------------------------------------------------------------
   --                    Program interfaces and resources                     --
   -----------------------------------------------------------------------------

   procedure Get_Program_Interface is new Loader.Getter_With_4_Params
     ("glGetProgramInterfaceiv", UInt, Enums.Program_Interface,
      Enums.Program_Interface_Param, Low_Level.Int_Array);

   function Get_Program_Resource_Index is new Loader.Function_With_3_Params
     ("glGetProgramResourceIndex", UInt, Enums.Program_Interface,
      C.char_array, UInt);

   procedure Get_Program_Resource_Name is new Loader.String_Getter_With_6_Params
     ("glGetProgramResourceName", Size, UInt, Enums.Program_Interface, UInt);

   function Get_Program_Resource is new Loader.Array_Getter_With_8_Params
     ("glGetProgramResourceiv", UInt, Enums.Program_Interface,
      UInt, Size, Enums.Program_Resource_Array, Int, Int_Array);

   function Get_Program_Resource_Location is new Loader.Function_With_3_Params
     ("glGetProgramResourceLocation", UInt, Enums.Program_Interface,
      C.char_array, Int);

   function Get_Program_Resource_Location_Index is new Loader.Function_With_3_Params
     ("glGetProgramResourceLocationIndex", UInt, Enums.Program_Interface,
      C.char_array, Int);

   -----------------------------------------------------------------------------
   --                                Pipelines                                --
   -----------------------------------------------------------------------------

   procedure Use_Program_Stages is new Loader.Procedure_With_3_Params
     ("glUseProgramStages", UInt, Low_Level.Bitfield, UInt);

   procedure Create_Program_Pipelines is new Loader.Getter_With_2_Params
     ("glCreateProgramPipelines", Size, UInt);

   procedure Delete_Program_Pipelines is new Loader.Array_Proc_With_2_Params
     ("glDeleteProgramPipelines", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Program_Pipeline is new Loader.Procedure_With_1_Param
     ("glBindProgramPipeline", UInt);

   procedure Get_Program_Pipeline_Param is new Loader.Getter_With_3_Params
     ("glGetProgramPipelineiv", UInt, Enums.Program_Pipeline_Param, Int);

   procedure Get_Program_Pipeline_Info_Log is new Loader.String_Getter_With_4_Params
     ("glGetProgramPipelineInfoLog", Size, UInt);

   procedure Validate_Program_Pipeline is new Loader.Procedure_With_1_Param
     ("glValidateProgramPipeline", UInt);

   function Create_Shader_Program is new Loader.Function_With_3_Params
     ("glCreateShaderProgramv", Objects.Shaders.Shader_Type, Size,
      Low_Level.CharPtr_Array, UInt);

   -----------------------------------------------------------------------------
   --                                 Queries                                 --
   -----------------------------------------------------------------------------

   procedure Create_Queries is new Loader.Getter_With_3_Params
     ("glCreateQueries", Objects.Queries.Async_Query_Type, Size, UInt);

   procedure Delete_Queries is new Loader.Array_Proc_With_2_Params
     ("glDeleteQueries", Size, UInt, Low_Level.UInt_Array);

   procedure Begin_Query_Indexed is new Loader.Procedure_With_3_Params
     ("glBeginQueryIndexed", Objects.Queries.Async_Query_Type, UInt, UInt);

   procedure End_Query_Indexed is new Loader.Procedure_With_2_Params
     ("glEndQueryIndexed", Objects.Queries.Async_Query_Type, UInt);

   procedure Begin_Conditional_Render is new Loader.Procedure_With_2_Params
     ("glBeginConditionalRender", UInt, Objects.Queries.Query_Mode);

   procedure End_Conditional_Render is new Loader.Procedure_Without_Params
     ("glEndConditionalRender");

   procedure Query_Counter is new Loader.Procedure_With_2_Params
     ("glQueryCounter", UInt, Objects.Queries.Timestamp_Query_Type);

   procedure Get_Query_Indexed_Param is new Loader.Getter_With_4_Params
     ("glGetQueryIndexed", Objects.Queries.Query_Type, UInt, Objects.Queries.Target_Param, Int);

   procedure Get_Query_Object_Int is new Loader.Getter_With_3_Params
     ("glGetQueryObjectiv", UInt, Objects.Queries.Query_Param, Int);

   procedure Get_Query_Object_UInt is new Loader.Getter_With_3_Params
     ("glGetQueryObjectuiv", UInt, Objects.Queries.Query_Param, UInt);

   -----------------------------------------------------------------------------
   --                                Samplers                                 --
   -----------------------------------------------------------------------------

   procedure Create_Samplers is new Loader.Getter_With_2_Params
     ("glCreateSamplers", Size, UInt);

   procedure Delete_Samplers is new Loader.Array_Proc_With_2_Params
     ("glDeleteSamplers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Sampler is new Loader.Procedure_With_2_Params
     ("glBindSampler", UInt, UInt);

   procedure Bind_Samplers is new Loader.Array_Proc_With_3_Params
     ("glBindSamplers", UInt, Size, UInt, Low_Level.UInt_Array);

   procedure Sampler_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Low_Level.Bool);

   procedure Sampler_Parameter_Float is new Loader.Procedure_With_3_Params
     ("glSamplerParameterf", UInt, Enums.Textures.Parameter, Single);

   procedure Sampler_Parameter_Floats is new Loader.Procedure_With_3_Params
     ("glSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   procedure Sampler_Parameter_Minifying_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Minifying_Function);

   procedure Sampler_Parameter_Magnifying_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Magnifying_Function);

   procedure Sampler_Parameter_Wrapping_Mode is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);

   procedure Sampler_Parameter_Compare_Kind is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);

   procedure Sampler_Parameter_Compare_Function is new Loader.Procedure_With_3_Params
     ("glSamplerParameteri", UInt, Enums.Textures.Parameter, Compare_Function);

   procedure Get_Sampler_Parameter_Bool is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Low_Level.Bool);

   procedure Get_Sampler_Parameter_Floats is new Loader.Getter_With_3_Params
     ("glGetSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   procedure Get_Sampler_Parameter_Minifying_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Minifying_Function);

   procedure Get_Sampler_Parameter_Magnifying_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Magnifying_Function);

   procedure Get_Sampler_Parameter_Wrapping_Mode is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Objects.Textures.Wrapping_Mode);

   procedure Get_Sampler_Parameter_Compare_Kind is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Enums.Textures.Compare_Kind);

   procedure Get_Sampler_Parameter_Compare_Function is new Loader.Getter_With_3_Params
     ("glGetSamplerParameteriv", UInt, Enums.Textures.Parameter, Compare_Function);

   -----------------------------------------------------------------------------
   --                           Transform feedback                            --
   -----------------------------------------------------------------------------

   procedure Transform_Feedback_Varyings is new Loader.Procedure_With_4_Params
     ("glTransformFeedbackVaryings", UInt, Size, Low_Level.CharPtr_Array,
      Objects.Transform_Feedbacks.Outputs_Format);

   procedure Create_Transform_Feedbacks is new Loader.Getter_With_2_Params
     ("glCreateTransformFeedbacks", Size, UInt);

   procedure Delete_Transform_Feedbacks is new Loader.Array_Proc_With_2_Params
     ("glDeleteTransformFeedbacks", Size, UInt, Low_Level.UInt_Array);

   procedure Begin_Transform_Feedback is new Loader.Procedure_With_1_Param
     ("glBeginTransformFeedback", Connection_Mode);

   procedure End_Transform_Feedback is new Loader.Procedure_Without_Params
     ("glEndTransformFeedback");

   procedure Pause_Transform_Feedback is new Loader.Procedure_Without_Params
     ("glPauseTransformFeedback");

   procedure Resume_Transform_Feedback is new Loader.Procedure_Without_Params
     ("glResumeTransformFeedback");

   procedure Bind_Transform_Feedback is new Loader.Procedure_With_2_Params
     ("glBindTransformFeedback", Enums.Transform_Feedback_Kind, UInt);

   procedure Transform_Feedback_Buffer_Base is new Loader.Procedure_With_3_Params
      ("glTransformFeedbackBufferBase", UInt, UInt, UInt);

   procedure Draw_Transform_Feedback is new Loader.Procedure_With_2_Params
     ("glDrawTransformFeedback", Connection_Mode, UInt);

   procedure Draw_Transform_Feedback_Instanced is new Loader.Procedure_With_3_Params
     ("glDrawTransformFeedbackInstanced", Connection_Mode, UInt, Size);

   procedure Draw_Transform_Feedback_Stream is new Loader.Procedure_With_3_Params
     ("glDrawTransformFeedbackStream", Connection_Mode, UInt, UInt);

   procedure Draw_Transform_Feedback_Stream_Instanced is new Loader.Procedure_With_4_Params
     ("glDrawTransformFeedbackStreamInstanced", Connection_Mode, UInt, UInt, Size);

   -----------------------------------------------------------------------------
   --                                Barriers                                 --
   -----------------------------------------------------------------------------

   procedure Texture_Barrier is new Loader.Procedure_Without_Params
     ("glTextureBarrier");

   procedure Memory_Barrier is new Loader.Procedure_With_1_Param
     ("glMemoryBarrier", Low_Level.Bitfield);

   procedure Memory_Barrier_By_Region is new Loader.Procedure_With_1_Param
     ("glMemoryBarrierByRegion", Low_Level.Bitfield);

   -----------------------------------------------------------------------------
   --                                  Debug                                  --
   -----------------------------------------------------------------------------

   procedure Debug_Message_Control is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Debug.Source, Debug.Message_Type,
      Debug.Severity, Size, UInt_Array, Low_Level.Bool);

   procedure Debug_Message_Control is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Debug.Source, Debug.Message_Type,
      Low_Level.Enum, Size, UInt_Array, Low_Level.Bool);

   procedure Debug_Message_Control is new Loader.Procedure_With_6_Params
     ("glDebugMessageControl", Low_Level.Enum, Low_Level.Enum,
      Debug.Severity, Size, UInt_Array, Low_Level.Bool);

   procedure Debug_Message_Insert is new Loader.Procedure_With_6_Params
     ("glDebugMessageInsert", Debug.Source, Debug.Message_Type,
      UInt, Debug.Severity, Size, C.char_array);

   procedure Debug_Message_Callback is new Loader.Procedure_With_2_Params
     ("glDebugMessageCallback", System.Address, System.Address);

   function Get_Debug_Message_Log is new Loader.Function_With_8_Params
     ("glGetDebugMessageLog", UInt, Size, Debug_Types.Source_Array_Access,
      Debug_Types.Type_Array_Access, Debug_Types.UInt_Array_Access,
      Debug_Types.Severity_Array_Access, Debug_Types.Size_Array_Access,
      Debug_Types.String_Access, UInt);

   procedure Push_Debug_Group is new Loader.Procedure_With_4_Params
     ("glPushDebugGroup", Debug.Source, UInt, Size, C.char_array);

   procedure Pop_Debug_Group is new Loader.Procedure_Without_Params
    ("glPopDebugGroup");

   procedure Object_Label is new Loader.Procedure_With_4_Params
     ("glObjectLabel", Types.Debug.Identifier, UInt, Size, C.char_array);

   procedure Get_Object_Label is new Loader.String_Getter_With_5_Params
     ("glGetObjectLabel", Size, Types.Debug.Identifier, UInt);

   procedure Get_Object_Label_Length is new Loader.Procedure_With_5_Params
     ("glGetObjectLabel", Types.Debug.Identifier, UInt, Size,
      Low_Level.Size_Access, C.Strings.chars_ptr);

   procedure Object_Pointer_Label is new Loader.Procedure_With_3_Params
     ("glObjectPtrLabel", System.Address, Size, C.char_array);
   --  TODO Use for Sync objects

   procedure Get_Object_Pointer_Label is new Loader.String_Getter_With_4_Params
     ("glGetObjectPtrLabel", Size, System.Address);
   --  TODO Use for Sync objects

   -----------------------------------------------------------------------------
   --                                 Syncing                                 --
   -----------------------------------------------------------------------------

   function Fence_Sync is new Loader.Function_With_2_Params
     ("glFenceSync", Low_Level.Enum, Low_Level.Bitfield, Low_Level.Sync);

   procedure Delete_Sync is new Loader.Procedure_With_1_Param
     ("glDeleteSync", Low_Level.Sync);

   function Get_Sync is new Loader.Array_Getter_With_5_Params
     ("glGetSynciv", Low_Level.Sync, Low_Level.Enum, Int, Int_Array);

   function Client_Wait_Sync is new Loader.Function_With_3_Params
     ("glClientWaitSync", Low_Level.Sync, Low_Level.Bitfield, Low_Level.UInt64,
      Fences.Wait_Status);

   procedure Wait_Sync is new Loader.Procedure_With_3_Params
     ("glWaitSync", Low_Level.Sync, Low_Level.Bitfield, Low_Level.UInt64);

   -----------------------------------------------------------------------------
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------

   procedure Depth_Range_Array is new Loader.Procedure_With_3_Params
     ("glDepthRangeArrayv", UInt, Size, Window.Depth_Range_List);

   procedure Viewport_Array is new Loader.Procedure_With_3_Params
     ("glViewportArrayv", UInt, Size, Window.Viewport_List);

   procedure Scissor_Array is new Loader.Procedure_With_3_Params
     ("glScissorArrayv", UInt, Size, Window.Scissor_Rectangle_List);

end GL.API;
