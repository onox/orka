--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.Runtime_Loading;

with GL.Attributes;
with GL.Blending;
with GL.Buffers;
with GL.Culling;
with GL.Enums.Getter;
with GL.Enums.Textures;
with GL.Errors;
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
with GL.Pixels;
with GL.Rasterization;
with GL.Toggles;
with GL.Types.Colors;

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

   function Get_Error return Errors.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Error,
                  External_Name => "glGetError");

   procedure Flush;
   pragma Import (Convention => StdCall, Entity => Flush,
                  External_Name => "glFlush");

   procedure Finish;
   pragma Import (Convention => StdCall, Entity => Finish,
                  External_Name => "glFinish");

   -----------------------------------------------------------------------------
   --                            Parameter getters                            --
   -----------------------------------------------------------------------------

   procedure Get_Boolean (Name   : Enums.Getter.Parameter;
                          Target : access Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Boolean,
                  External_Name => "glGetBooleanv");

   procedure Get_Double (Name   : Enums.Getter.Parameter;
                         Target : access Double);
   pragma Import (Convention => StdCall, Entity => Get_Double,
                  External_Name => "glGetDoublev");

   procedure Get_Double_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Doubles.Vector2);
   pragma Import (Convention => StdCall, Entity => Get_Double_Vec2,
                  External_Name => "glGetDoublev");
   
   procedure Get_Single (Name : Enums.Getter.Parameter;
                         Target : access Single);
   pragma Import (Convention => StdCall, Entity => Get_Single,
                  External_Name => "glGetFloatv");
   
   procedure Get_Single_Vec2 (Name   : Enums.Getter.Parameter;
                              Target : in out Singles.Vector2);
   pragma Import (Convention => StdCall, Entity => Get_Single_Vec2,
                  External_Name => "glGetFloatv");
   
   procedure Get_Color (Name : Enums.Getter.Parameter;
                        Target : in out Colors.Color);
   pragma Import (Convention => StdCall, Entity => Get_Color,
                  External_Name => "glGetFloatv");

   procedure Get_Long (Name : Enums.Getter.Parameter;
                       Target : access Long);
   pragma Import (Convention => StdCall, Entity => Get_Long,
                  External_Name => "glGetInteger64v");

   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access Int);
   pragma Import (Convention => StdCall, Entity => Get_Integer,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Int_Vec4 (Name   : Enums.Getter.Parameter;
                           Target : in out Ints.Vector4);
   pragma Import (Convention => StdCall, Entity => Get_Int_Vec4,
                  External_Name => "glGetIntegerv");

   procedure Get_Unsigned_Integer (Name   : Enums.Getter.Parameter;
                                   Target : access UInt);
   pragma Import (Convention => StdCall, Entity => Get_Unsigned_Integer,
                  External_Name => "glGetIntegerv");

   procedure Get_Size (Name   : Enums.Getter.Parameter;
                       Target : access Size);
   pragma Import (Convention => StdCall, Entity => Get_Size,
                  External_Name => "glGetIntegerv");

   procedure Get_Blend_Factor (Name : Enums.Getter.Parameter;
                               Target : access Blending.Blend_Factor);
   pragma Import (Convention => StdCall, Entity => Get_Blend_Factor,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Alignment (Name : Enums.Getter.Parameter;
                            Target : access Pixels.Alignment);
   pragma Import (Convention => StdCall, Entity => Get_Alignment,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Blend_Equation (Name : Enums.Getter.Parameter;
                                 Target : access Blending.Equation);
   pragma Import (Convention => StdCall, Entity => Get_Blend_Equation,
                  External_Name => "glGetIntegerv");

   procedure Get_Compare_Function (Name : Enums.Getter.Parameter;
                                   Target : access Compare_Function);
   pragma Import (Convention => StdCall, Entity => Get_Compare_Function,
                  External_Name => "glGetIntegerv");

   procedure Get_Orientation (Name : Enums.Getter.Parameter;
                              Target : access Culling.Orientation);
   pragma Import (Convention => StdCall, Entity => Get_Orientation,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Face_Selector (Name : Enums.Getter.Parameter;
                                Target : access Culling.Face_Selector);
   pragma Import (Convention => StdCall, Entity => Get_Face_Selector,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Polygon_Mode (Name   : Enums.Getter.Parameter;
                               Target : access Rasterization.Polygon_Mode_Type);
   pragma Import (Convention => StdCall, Entity => Get_Polygon_Mode,
                  External_Name => "glGetIntegerv");
   
   procedure Get_Logic_Op (Name : Enums.Getter.Parameter;
                           Target : access Framebuffer.Logic_Op);
   pragma Import (Convention => StdCall, Entity => Get_Logic_Op,
                  External_Name => "glGetIntegerv");

   procedure Get_Stencil_Action (Name : Enums.Getter.Parameter;
                                 Target : access Buffers.Stencil_Action);
   pragma Import (Convention => StdCall, Entity => Get_Stencil_Action,
                  External_Name => "glGetIntegerv");

   procedure Get_Read_Buffer_Selector
     (Name   : Enums.Getter.Parameter;
      Target : access Buffers.Color_Buffer_Selector);
   pragma Import (Convention => StdCall, Entity => Get_Read_Buffer_Selector,
                  External_Name => "glGetIntegerv");
   
   function Get_String (Name : Enums.Getter.String_Parameter) 
                        return C.Strings.chars_ptr;  
   pragma Import (Convention => StdCall, Entity => Get_String,
                  External_Name => "glGetString");
   
   function Get_String_I is new Loader.Function_With_2_Params
     ("glGetStringi", Enums.Getter.String_Parameter, UInt, C.Strings.chars_ptr);

   -----------------------------------------------------------------------------
   --                                 Toggles                                 --
   -----------------------------------------------------------------------------

   procedure Enable (Subject : Toggles.Toggle);
   pragma Import (Convention => StdCall, Entity => Enable,
                  External_Name => "glEnable");

   procedure Disable (Subject : Toggles.Toggle);
   pragma Import (Convention => StdCall, Entity => Disable,
                  External_Name => "glDisable");

   function Is_Enabled (Subject : Toggles.Toggle) return Low_Level.Bool;
   pragma Import (Convention => StdCall, Entity => Is_Enabled,
                  External_Name => "glIsEnabled");
   
   -----------------------------------------------------------------------------
   --                                 Culling                                 --
   -----------------------------------------------------------------------------
   
   procedure Cull_Face (Selector : Culling.Face_Selector);
   pragma Import (Convention => StdCall, Entity => Cull_Face,
                  External_Name => "glCullFace");
   
   procedure Front_Face (Face : Culling.Orientation);
   pragma Import (Convention => StdCall, Entity => Front_Face,
                  External_Name => "glFrontFace");
   
   -----------------------------------------------------------------------------
   --                               Pixel stuff                               --
   -----------------------------------------------------------------------------
   
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Low_Level.Bool);
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Size);
   procedure Pixel_Store (Param : Enums.Pixel_Store_Param;
                          Value : Pixels.Alignment);
   pragma Import (Convention => StdCall, Entity => Pixel_Store,
                  External_Name => "glPixelStorei");
   
   -----------------------------------------------------------------------------
   --                         Framebuffer operations                          --
   -----------------------------------------------------------------------------
   
   procedure Clamp_Color is new Loader.Procedure_With_2_Params
     ("glClampColor", Enums.Clamp_Color_Param, Low_Level.Bool);
   
   -----------------------------------------------------------------------------
   --                                 Drawing                                 --
   -----------------------------------------------------------------------------

   procedure Draw_Arrays (Mode  : Connection_Mode;
                          First : Int; Count : Size);
   pragma Import (Convention => StdCall, Entity => Draw_Arrays,
                  External_Name => "glDrawArrays");

   procedure Draw_Arrays_Instanced is new Loader.Procedure_With_4_Params
     ("glDrawArraysInstanced", Connection_Mode, Int, Size, Size);

   procedure Multi_Draw_Arrays is new Loader.Procedure_With_4_Params
     ("glMultiDrawArrays", Connection_Mode, Size_Array, Size_Array, Size);

   procedure Draw_Elements (Mode       : Connection_Mode;
                            Count      : Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Indices    : Zero);
   pragma Import (Convention => StdCall, Entity => Draw_Elements,
                  External_Name => "glDrawElements");

   procedure Draw_Elements_Instanced is new Loader.Procedure_With_5_Params
     ("glDrawElementsInstanced", Connection_Mode, Size, Unsigned_Numeric_Type, Zero, Size);

   procedure Draw_Elements_Instanced_Base_Vertex is new Loader.Procedure_With_6_Params
     ("glDrawElementsInstancedBaseVertex", Connection_Mode, Size,
      Unsigned_Numeric_Type, Int, Size, Int);

   procedure Draw_Elements_Base_Vertex is new Loader.Procedure_With_5_Params
     ("glDrawElementsBaseVertex", Connection_Mode, Size, Unsigned_Numeric_Type, Int, Int);

   procedure Multi_Draw_Elements is new Loader.Procedure_With_5_Params
     ("glMultiDrawElements", Connection_Mode, Size_Array, Unsigned_Numeric_Type, Zero, Size);

   procedure Multi_Draw_Elements_Base_Vertex is new Loader.Procedure_With_6_Params
     ("glMultiDrawElementsBaseVertex", Connection_Mode, Size_Array,
      Unsigned_Numeric_Type, Int_Array, Size, Int_Array);

   -----------------------------------------------------------------------------
   --                                Blending                                 --
   -----------------------------------------------------------------------------

   procedure Blend_Func (Src_Factor, Dst_Factor : Blending.Blend_Factor);
   pragma Import (Convention => StdCall, Entity => Blend_Func,
                  External_Name => "glBlendFunc");
   
   procedure Blend_Func_I is new Loader.Procedure_With_3_Params
     ("glBlendFunci", Buffers.Draw_Buffer_Index, Blending.Blend_Factor,
      Blending.Blend_Factor);
   
   procedure Blend_Func_Separate is new Loader.Procedure_With_4_Params
     ("glBlendFuncSeparate", Blending.Blend_Factor, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor);
   
   procedure Blend_Func_Separate_I is new Loader.Procedure_With_5_Params
     ("glBlendFuncSeparate", Buffers.Draw_Buffer_Index, Blending.Blend_Factor,
      Blending.Blend_Factor, Blending.Blend_Factor, Blending.Blend_Factor);
   
   procedure Blend_Color is new Loader.Procedure_With_4_Params
     ("glBlendColor", Colors.Component, Colors.Component, Colors.Component, 
      Colors.Component);
   
   procedure Blend_Equation (Mode : Blending.Equation);
   pragma Import (Convention => StdCall, Entity => Blend_Equation,
                  External_Name => "glBlendEquation");
   
   procedure Blend_Equation_I is new Loader.Procedure_With_2_Params
     ("glBlendEquationi", Buffers.Draw_Buffer_Index, Blending.Equation);
   
   procedure Blend_Equation_Separate is new Loader.Procedure_With_2_Params
     ("glBlendEquationSeparate", Blending.Equation, Blending.Equation);
   
   procedure Blend_Equation_Separate_I is new Loader.Procedure_With_3_Params
     ("glBlendEquationi", Buffers.Draw_Buffer_Index, Blending.Equation,
      Blending.Equation);
   
   -----------------------------------------------------------------------------
   --                              Rasterization                              --
   -----------------------------------------------------------------------------
   
   procedure Line_Width (Value : Single);
   pragma Import (Convention => StdCall, Entity => Line_Width,
                  External_Name => "glLineWidth");
   
   procedure Polygon_Mode (Face : Culling.Face_Selector;
                           Value : Rasterization.Polygon_Mode_Type);
   pragma Import (Convention => StdCall, Entity => Polygon_Mode,
                  External_Name => "glPolygonMode");
   
   -----------------------------------------------------------------------------
   --                                 Buffers                                 --
   -----------------------------------------------------------------------------

   procedure Clear (Bits : Low_Level.Bitfield);
   pragma Import (Convention => StdCall, Entity => Clear,
                  External_Name => "glClear");

   procedure Clear_Color (Red, Green, Blue, Alpha : Colors.Component);
   pragma Import (Convention => StdCall, Entity => Clear_Color,
                  External_Name => "glClearColor");

   procedure Clear_Depth (Depth : Buffers.Depth);
   pragma Import (Convention => StdCall, Entity => Clear_Depth,
                  External_Name => "glClearDepth");

   procedure Clear_Stencil (Index : Buffers.Stencil_Index);
   pragma Import (Convention => StdCall, Entity => Clear_Stencil,
                  External_Name => "glClearStencil");

   -----------------------------------------------------------------------------
   --                        Depth and stencil buffers                        --
   -----------------------------------------------------------------------------

   procedure Depth_Mask (Value : Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Depth_Mask,
                  External_Name => "glDepthMask");

   procedure Depth_Func (Func : Compare_Function);
   pragma Import (Convention => StdCall, Entity => Depth_Func,
                  External_Name => "glDepthFunc");

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

   procedure Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                  Param_Name : Enums.Textures.Parameter;
                                  Value      : Single);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Float,
                  External_Name => "glTexParameterf");

   procedure Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                Param_Name : Enums.Textures.Parameter;
                                Value      : Int);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Int,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Min_Filter (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Minifying_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Min_Filter,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Mag_Filter (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Magnifying_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Mag_Filter,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Wrap_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Objects.Textures.Wrapping_Mode);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Wrap_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Comp_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Enums.Textures.Compare_Kind);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Comp_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : Compare_Function);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Comp_Func,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Depth_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Value      : Objects.Textures.Depth_Mode);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Depth_Mode,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                 Param_Name : Enums.Textures.Parameter;
                                 Value      : Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Bool,
                  External_Name => "glTexParameteri");

   procedure Tex_Parameter_Floats (Target     : Low_Level.Enums.Texture_Kind;
                                   Param_Name : Enums.Textures.Parameter;
                                   Values     : Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Tex_Parameter_Floats,
                  External_Name => "glTexParameterfv");

   procedure Get_Tex_Parameter_Float (Target     : Low_Level.Enums.Texture_Kind;
                                      Param_Name : Enums.Textures.Parameter;
                                      Value      : out Single);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Float,
                  External_Name => "glGetTexParameterfv");
   
   procedure Get_Tex_Parameter_Floats (Target     : Low_Level.Enums.Texture_Kind;
                                       Param_Name : Enums.Textures.Parameter;
                                       Values     : in out Low_Level.Single_Array);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Floats,
                  External_Name => "glGetTexParameterfv");

   procedure Get_Tex_Parameter_Int (Target     : Low_Level.Enums.Texture_Kind;
                                    Param_Name : Enums.Textures.Parameter;
                                    Values     : out Int);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Int,
                  External_Name => "glGetTexParameteriv");
   
   procedure Get_Tex_Parameter_Ints (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : in out Low_Level.Int_Array);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Ints,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Wrap_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Objects.Textures.Wrapping_Mode);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Wrap_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Comp_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Enums.Textures.Compare_Kind);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Comp_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Comp_Func (Target     : Low_Level.Enums.Texture_Kind;
                                          Param_Name : Enums.Textures.Parameter;
                                          Values     : out Compare_Function);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Comp_Func,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Depth_Mode (Target     : Low_Level.Enums.Texture_Kind;
                                           Param_Name : Enums.Textures.Parameter;
                                           Values     : out Objects.Textures.Depth_Mode);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Depth_Mode,
                  External_Name => "glGetTexParameteriv");

   procedure Get_Tex_Parameter_Bool (Target     : Low_Level.Enums.Texture_Kind;
                                     Param_Name : Enums.Textures.Parameter;
                                     Values     : out Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Parameter_Bool,
                  External_Name => "glGetTexParameteriv");
   
   procedure Get_Tex_Level_Parameter_Size
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Size);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Level_Parameter_Size,
                  External_Name => "glGetTexLevelParameteriv");
   
   procedure Get_Tex_Level_Parameter_Format
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Internal_Format);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Level_Parameter_Format,
                  External_Name => "glGetTexLevelParameteriv");
   
   procedure Get_Tex_Level_Parameter_Type
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Pixels.Channel_Data_Type);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Level_Parameter_Type,
                  External_Name => "glGetTexLevelParameteriv");
   
   procedure Get_Tex_Level_Parameter_Bool
     (Target     : Low_Level.Enums.Texture_Kind;
      Level      : Objects.Textures.Mipmap_Level;
      Param_Name : Enums.Textures.Level_Parameter;
      Value      : out Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Tex_Level_Parameter_Bool,
                  External_Name => "glGetTexLevelParameteriv");

   procedure Gen_Textures (N : Size; Textures : access UInt);
   pragma Import (Convention => StdCall, Entity => Gen_Textures,
                  External_Name => "glGenTextures");

   procedure Bind_Texture (Target  : Low_Level.Enums.Texture_Kind;
                           Texture : UInt);
   pragma Import (Convention => StdCall, Entity => Bind_Texture,
                  External_Name => "glBindTexture");

   procedure Delete_Textures (N : Size; Textures : Low_Level.UInt_Array);
   pragma Import (Convention => StdCall, Entity => Delete_Textures,
                  External_Name => "glDeleteTextures");

   procedure Tex_Image_1D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width  : Size;
                           Border : Int;
                           Format : Pixels.Format;
                           Data_Type : Pixels.Data_Type;
                           Data   : System.Address);
   pragma Import (Convention => StdCall, Entity => Tex_Image_1D,
                  External_Name => "glTexImage1D");
   
   procedure Tex_Image_2D (Target : Low_Level.Enums.Texture_Kind;
                           Level  : Objects.Textures.Mipmap_Level;
                           Internal_Format : Pixels.Internal_Format;
                           Width, Height : Size;
                           Border : Int;
                           Format : Pixels.Format;
                           Data_Type : Pixels.Data_Type;
                           Data : System.Address);
   pragma Import (Convention => StdCall, Entity => Tex_Image_2D,
                  External_Name => "glTexImage2D");

   procedure Tex_Image_3D is new Loader.Procedure_With_10_Params
     ("glTexImage3D", Low_Level.Enums.Texture_Kind,
      Objects.Textures.Mipmap_Level, Pixels.Internal_Format, Size, Size,
      Size, Int, Pixels.Format, Pixels.Data_Type, System.Address);
   
   procedure Active_Texture is new Loader.Procedure_With_1_Param
     ("glActiveTexture", Int);
   
   procedure Generate_Mipmap is new Loader.Procedure_With_1_Param
     ("glGenerateMipmap", Low_Level.Enums.Texture_Kind);
   
   procedure Invalidate_Tex_Image is new Loader.Procedure_With_2_Params
     ("glInvalidateTexImage", UInt, Objects.Textures.Mipmap_Level);
   
   procedure Invalidate_Tex_Sub_Image is new Loader.Procedure_With_8_Params
     ("glInvalidateTexSubImage", UInt, Objects.Textures.Mipmap_Level,
      Int, Int, Int, Size, Size, Size);

   -----------------------------------------------------------------------------
   --                             Buffer objects                              --
   -----------------------------------------------------------------------------

   procedure Create_Buffers is new Loader.Getter_With_2_Params
      ("glCreateBuffers", Size, UInt);

   procedure Delete_Buffers is new Loader.Array_Proc_With_2_Params
      ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Buffer is new Loader.Procedure_With_2_Params
      ("glBindBuffer", Low_Level.Enums.Buffer_Kind, UInt);

   procedure Bind_Buffer_Base is new Loader.Procedure_With_3_Params
      ("glBindBufferBase", Low_Level.Enums.Buffer_Kind, UInt, UInt);

   procedure Named_Buffer_Data is new Loader.Procedure_With_4_Params
      ("glNamedBufferData", UInt, Low_Level.SizeIPtr,
       System.Address, Objects.Buffers.Buffer_Usage);

   procedure Named_Buffer_Sub_Data is new Loader.Procedure_With_4_Params
      ("glNamedBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr,
       System.Address);

   procedure Named_Buffer_Storage is new Loader.Procedure_With_4_Params
      ("glNamedBufferStorage", UInt, Low_Level.SizeIPtr,
       System.Address, Low_Level.Bitfield);

   -- glMapNamedBuffer[Range] returns an instance of generic Interfaces.C.Pointers.Pointer,
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
     ("glGetNamedBufferParameteriv", UInt, Enums.Buffer_Param,
      Size);

   procedure Get_Named_Buffer_Parameter_Usage is new Loader.Getter_With_3_Params
     ("glGetNamedBufferParameteriv", UInt, Enums.Buffer_Param,
      Objects.Buffers.Buffer_Usage);

   procedure Invalidate_Buffer_Data is new Loader.Procedure_With_1_Param
     ("glInvalidateBufferData", UInt);

   procedure Invalidate_Buffer_Sub_Data is new Loader.Procedure_With_3_Params
     ("glInvalidateBufferSubData", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

   procedure Flush_Mapped_Named_Buffer_Range is new Loader.Procedure_With_3_Params
     ("glFlushMappedNamedBufferRange", UInt, Low_Level.IntPtr, Low_Level.SizeIPtr);

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
     ("glVertexArrayAttribFormat", UInt, Attributes.Attribute, Component_Count,
      Numeric_Type, Low_Level.Bool, UInt);

   procedure Vertex_Array_AttribI_Format is new Loader.Procedure_With_5_Params
     ("glVertexArrayAttribIFormat", UInt, Attributes.Attribute, Component_Count,
      Numeric_Type, UInt);

   procedure Vertex_Array_AttribL_Format is new Loader.Procedure_With_5_Params
     ("glVertexArrayAttribLFormat", UInt, Attributes.Attribute, Component_Count,
      Numeric_Type, UInt);

   procedure Vertex_Array_Attrib_Binding is new Loader.Procedure_With_3_Params
     ("glVertexArrayAttribBinding", UInt, Attributes.Attribute, Objects.Vertex_Arrays.Binding);

   procedure Vertex_Array_Binding_Divisor is new Loader.Procedure_With_3_Params
     ("glVertexArrayBindingDivisor", UInt, Objects.Vertex_Arrays.Binding, UInt);

   procedure Vertex_Array_Vertex_Buffer is new Loader.Procedure_With_5_Params
     ("glVertexArrayVertexBuffer", UInt, Objects.Vertex_Arrays.Binding, UInt, Int, Size);

   procedure Vertex_Array_Element_Buffer is new Loader.Procedure_With_2_Params
     ("glVertexArrayElementBuffer", UInt, UInt);

   procedure Enable_Vertex_Array_Attrib is new Loader.Procedure_With_2_Params
     ("glEnableVertexArrayAttrib", UInt, Attributes.Attribute);

   procedure Disable_Vertex_Array_Attrib is new Loader.Procedure_With_2_Params
     ("glDisableVertexArrayAttrib", UInt, Attributes.Attribute);

   -----------------------------------------------------------------------------
   --                           Renderbuffer objects                          --
   -----------------------------------------------------------------------------

   procedure Create_Renderbuffers is new Loader.Getter_With_2_Params
     ("glCreateRenderbuffers", Size, UInt);

   procedure Delete_Renderbuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteBuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Named_Renderbuffer_Storage is new Loader.Procedure_With_4_Params
     ("glNamedRenderbufferStorage", UInt,
      Pixels.Internal_Format, Size, Size);

   procedure Named_Renderbuffer_Storage_Multisample is new Loader.Procedure_With_5_Params
     ("glNamedRenderbufferStorageMultisample", UInt, Size,
      Pixels.Internal_Format, Size, Size);

   procedure Bind_Renderbuffer is new Loader.Procedure_With_2_Params
     ("glBindRenderbuffer", Low_Level.Enums.Renderbuffer_Kind, UInt);

   procedure Get_Named_Renderbuffer_Parameter_Int is new Loader.Getter_With_3_Params
     ("glGetNamedRenderbufferParameteriv", UInt,
      Enums.Getter.Renderbuffer_Parameter, Int);

   procedure Get_Named_Renderbuffer_Parameter_Internal_Format is new
     Loader.Getter_With_3_Params ("glGetNamedRenderbufferParameteriv",
                                  UInt, Enums.Getter.Renderbuffer_Parameter,
                                  Pixels.Internal_Format);

   -----------------------------------------------------------------------------
   --                    Framebuffer objects and handling                     --
   -----------------------------------------------------------------------------

   procedure Read_Pixels (X, Y : Int; Width, Height : Size;
                          Format : Pixels.Format; Data_Type : Pixels.Data_Type;
                          Data : System.Address);
   pragma Import (Convention => StdCall, Entity => Read_Pixels,
                  External_Name => "glReadPixels");

   procedure Logic_Op (Value : Framebuffer.Logic_Op);
   pragma Import (Convention => StdCall, Entity => Logic_Op,
                  External_Name => "glLogicOp");

   procedure Create_Framebuffers is new Loader.Getter_With_2_Params
     ("glCreateFramebuffers", Size, UInt);

   procedure Delete_Framebuffers is new Loader.Array_Proc_With_2_Params
     ("glDeleteFramebuffers", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Framebuffer is new Loader.Procedure_With_2_Params
     ("glBindFramebuffer", Low_Level.Enums.Framebuffer_Kind, UInt);

   procedure Named_Framebuffer_Draw_Buffer is new Loader.Procedure_With_2_Params
     ("glNamedFramebufferDrawBuffer", UInt, Buffers.Color_Buffer_Selector);

   procedure Named_Framebuffer_Draw_Buffers is new Loader.Procedure_With_3_Params
     ("glNamedFramebufferDrawBuffers", UInt, Size, Buffers.Color_Buffer_List);

   procedure Named_Framebuffer_Read_Buffer is new Loader.Procedure_With_2_Params
     ("glNamedFramebufferReadBuffer", UInt, Buffers.Color_Buffer_Selector);

   function Check_Named_Framebuffer_Status is new Loader.Function_With_1_Param
     ("glCheckNamedFramebufferStatus", UInt,
      Objects.Framebuffers.Framebuffer_Status);

   procedure Named_Framebuffer_Renderbuffer is new Loader.Procedure_With_4_Params
     ("glNamedFramebufferRenderbuffer", UInt, Objects.Framebuffers.Attachment_Point,
      Low_Level.Enums.Renderbuffer_Kind, UInt);

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

   procedure Clear_Named_Framebuffer_Color is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Low_Level.Enums.Only_Color_Buffer,
      Buffers.Draw_Buffer_Index, Colors.Color);

   type Depth_Pointer is access constant Buffers.Depth;
   procedure Clear_Named_Framebuffer_Depth is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferfv", UInt, Low_Level.Enums.Only_Depth_Buffer,
      Zero, Depth_Pointer);

   type Stencil_Pointer is access constant Buffers.Stencil_Index;
   procedure Clear_Named_Framebuffer_Stencil is new Loader.Procedure_With_4_Params
     ("glClearNamedFramebufferiv", UInt, Low_Level.Enums.Only_Stencil_Buffer,
      Zero, Stencil_Pointer);

   procedure Clear_Named_Framebuffer_Depth_Stencil is new Loader.Procedure_With_5_Params
     ("glClearNamedFramebufferfi", UInt, Low_Level.Enums.Only_Depth_Stencil_Buffer,
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

   procedure Program_Parameter_Bool is new Loader.Procedure_With_3_Params
     ("glProgramParameteri", UInt, Enums.Program_Set_Param, Low_Level.Bool);

   procedure Attach_Shader is new Loader.Procedure_With_2_Params
     ("glAttachShader", UInt, UInt);

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

   procedure Bind_Attrib_Location is new Loader.Procedure_With_3_Params
     ("glBindAttribLocation", UInt, Attributes.Attribute, C.char_array);

   function Get_Attrib_Location is new Loader.Function_With_2_Params
     ("glGetAttribLocation", UInt, C.char_array, Attributes.Attribute);

   function Get_Attached_Shaders is new Loader.Array_Getter_With_4_Params
     ("glGetAttachedShaders", UInt, UInt, UInt_Array);

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
     ("glUseProgramStages", UInt, Objects.Shaders.Shader_Type, UInt);

   procedure Create_Program_Pipelines is new Loader.Getter_With_2_Params
     ("glCreateProgramPipelines", Size, UInt);

   procedure Delete_Program_Pipelines is new Loader.Array_Proc_With_2_Params
     ("glDeleteProgramPipelines", Size, UInt, Low_Level.UInt_Array);

   procedure Bind_Program_Pipeline is new Loader.Procedure_With_1_Param
     ("glBindProgramPipeline", UInt);

   procedure Active_Shader_Program is new Loader.Procedure_With_2_Params
     ("glActiveShaderProgram", UInt, UInt);

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

   procedure Gen_Queries is new Loader.Getter_With_2_Params
     ("glGenQueries", Size, UInt);

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

   procedure Sampler_Parameter_Float is new Loader.Procedure_With_3_Params
     ("glSamplerParameterf", UInt, Enums.Textures.Parameter, Single);

   procedure Sampler_Parameter_Floats is new Loader.Procedure_With_3_Params
     ("glSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

   procedure Get_Sampler_Parameter_Floats is new Loader.Getter_With_3_Params
     ("glGetSamplerParameterfv", UInt, Enums.Textures.Parameter, Low_Level.Single_Array);

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
     ("glBindTransformFeedback", Low_Level.Enums.Transform_Feedback_Kind, UInt);

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
   --                  Transformation to window coordinates                   --
   -----------------------------------------------------------------------------
   
   procedure Depth_Range (Near, Far : Double);
   pragma Import (Convention => StdCall, Entity => Depth_Range,
                  External_Name => "glDepthRange");
   
   procedure Viewport (X, Y : Int; Width, Height : Size);
   pragma Import (Convention => StdCall, Entity => Viewport,
                  External_Name => "glViewport");
   
end GL.API;
