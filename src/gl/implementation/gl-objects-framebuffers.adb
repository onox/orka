--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with Ada.Containers.Indefinite_Holders;
with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Framebuffers is

   function Status (Object : Framebuffer) return Framebuffer_Status is
   begin
      return API.Check_Named_Framebuffer_Status (Object.Reference.GL_Id);
   end Status;

   procedure Set_Active_Buffer (Object   : Framebuffer;
                                Selector : Buffers.Color_Buffer_Selector) is
   begin
      API.Named_Framebuffer_Draw_Buffer (Object.Reference.GL_Id, Selector);
      Raise_Exception_On_OpenGL_Error;
   end Set_Active_Buffer;

   procedure Set_Active_Buffers (Object : Framebuffer;
                                 List   : Buffers.Color_Buffer_List) is
   begin
      API.Named_Framebuffer_Draw_Buffers (Object.Reference.GL_Id, List'Length, List);
      Raise_Exception_On_OpenGL_Error;
   end Set_Active_Buffers;

   procedure Set_Read_Buffer (Object : Framebuffer; Selector : Buffers.Color_Buffer_Selector) is
   begin
      API.Named_Framebuffer_Read_Buffer (Object.Reference.GL_Id, Selector);
      Raise_Exception_On_OpenGL_Error;
   end Set_Read_Buffer;

   procedure Attach_Renderbuffer (Object : Framebuffer; Attachment : Attachment_Point;
                                  Render_Object : Renderbuffers.Renderbuffer'Class) is
   begin
      API.Named_Framebuffer_Renderbuffer (Object.Reference.GL_Id, Attachment,
                                          Low_Level.Enums.Renderbuffer,
                                          Render_Object.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Attach_Renderbuffer;

   procedure Attach_Texture (Object : Framebuffer;
                             Attachment : Attachment_Point;
                             Texture_Object : Textures.Texture'Class;
                             Level : Textures.Mipmap_Level) is
   begin
      API.Named_Framebuffer_Texture (Object.Reference.GL_Id, Attachment,
                                     Texture_Object.Raw_Id, Level);
      Raise_Exception_On_OpenGL_Error;
   end Attach_Texture;

   procedure Attach_Texture_Layer (Object : Framebuffer;
                                   Attachment : Attachment_Point;
                                   Texture_Object : Textures.Texture'Class;
                                   Level : Textures.Mipmap_Level;
                                   Layer : Natural) is
   begin
      API.Named_Framebuffer_Texture_Layer (Object.Reference.GL_Id, Attachment,
                                           Texture_Object.Raw_Id, Level, Int (Layer));
      Raise_Exception_On_OpenGL_Error;
   end Attach_Texture_Layer;

   procedure Invalidate_Data (Object : Framebuffer;
                              Attachments : Attachment_List) is
   begin
      API.Invalidate_Named_Framebuffer_Data (Object.Reference.GL_Id, Attachments'Length, Attachments);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Data;

   procedure Invalidate_Sub_Data (Object        : Framebuffer;
                                  Attachments   : Attachment_List;
                                  X, Y          : Int;
                                  Width, Height : Size) is
   begin
      API.Invalidate_Named_Framebuffer_Sub_Data (Object.Reference.GL_Id, Attachments'Length,
                                                 Attachments, X, Y, Width, Height);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Sub_Data;

   procedure Blit (Read_Object, Draw_Object : Framebuffer;
                   Src_X0, Src_Y0, Src_X1, Src_Y1,
                   Dst_X0, Dst_Y0, Dst_X1, Dst_Y1 : Int;
                   Mask : Buffers.Buffer_Bits;
                   Filter : Textures.Magnifying_Function) is
      use type Low_Level.Bitfield;
      function Convert is new Ada.Unchecked_Conversion
        (Buffers.Buffer_Bits, Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield
        := Convert (Mask) and 2#0100010100000000#;

   begin
      API.Blit_Named_Framebuffer (Read_Object.Reference.GL_Id,
                                  Draw_Object.Reference.GL_Id,
                                  Src_X0, Src_Y0, Src_X1, Src_Y1,
                                  Dst_X0, Dst_Y0, Dst_X1, Dst_Y1,
                                  Raw_Bits, Filter);
      Raise_Exception_On_OpenGL_Error;
   end Blit;

   procedure Set_Default_Width (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Width, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Default_Width;

   function Default_Width (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Width, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Default_Width;

   function Max_Framebuffer_Width return Size is
      Ret : aliased Size;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Width, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Max_Framebuffer_Width;

   procedure Set_Default_Height (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Height,
                                            Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Default_Height;

   function Default_Height (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Height, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Default_Height;

   function Max_Framebuffer_Height return Size is
      Ret : aliased Size;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Height, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Max_Framebuffer_Height;

   procedure Set_Default_Layers (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Layers, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Default_Layers;

   function Default_Layers (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Layers, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Default_Layers;

   function Max_Framebuffer_Layers return Size is
      Ret : aliased Size;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Layers, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Max_Framebuffer_Layers;

   procedure Set_Default_Samples (Object : Framebuffer; Value : Size) is
   begin
      API.Named_Framebuffer_Parameter_Size (Object.Reference.GL_Id,
                                            Enums.Default_Samples,
                                            Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Default_Samples;

   function Default_Samples (Object : Framebuffer) return Size is
      Ret : aliased Size;
   begin
      API.Get_Named_Framebuffer_Parameter_Size
        (Object.Reference.GL_Id, Enums.Default_Samples, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Default_Samples;

   function Max_Framebuffer_Samples return Size is
      Ret : aliased Size;
   begin
      API.Get_Size (Enums.Getter.Max_Framebuffer_Samples, Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Max_Framebuffer_Samples;

   procedure Set_Default_Fixed_Sample_Locations (Object : Framebuffer; Value : Boolean) is
   begin
      API.Named_Framebuffer_Parameter_Bool (Object.Reference.GL_Id,
                                            Enums.Default_Fixed_Sample_Locations,
                                            Low_Level.Bool (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Default_Fixed_Sample_Locations;

   function Default_Fixed_Sample_Locations (Object : Framebuffer) return Boolean is
      Ret : aliased Low_Level.Bool;
   begin
      API.Get_Named_Framebuffer_Parameter_Bool
        (Object.Reference.GL_Id, Enums.Default_Fixed_Sample_Locations,
         Ret'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Default_Fixed_Sample_Locations;

   overriding
   procedure Initialize_Id (Object : in out Framebuffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Framebuffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Framebuffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Framebuffers (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   package Framebuffer_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Framebuffer'Class);

   type Framebuffer_Target_Array is array (Low_Level.Enums.Framebuffer_Kind) of Framebuffer_Holder.Holder;
   Current_Framebuffers : Framebuffer_Target_Array;

   procedure Bind (Target : Framebuffer_Target;
                   Object : Framebuffer'Class) is
      Holder : Framebuffer_Holder.Holder := Current_Framebuffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Framebuffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Holder.Replace_Element (Object);
      end if;
   end Bind;

   function Current (Target : Framebuffer_Target) return Framebuffer'Class is
      Holder : constant Framebuffer_Holder.Holder := Current_Framebuffers (Target.Kind);
   begin
      if Holder.Is_Empty then
         raise No_Object_Bound_Exception with GL.Low_Level.Enums.Framebuffer_Kind'Image (Target.Kind);
      else
         return Holder.Element;
      end if;
   end Current;

   procedure Clear_Color_Buffer (Object : Framebuffer;
                                 Index  : Buffers.Draw_Buffer_Index;
                                 Value  : Colors.Color) is
   begin
      API.Clear_Named_Framebuffer_Color (Object.Reference.GL_Id,
                                         Low_Level.Enums.Color, Index, Value);
      Raise_Exception_On_OpenGL_Error;
   end Clear_Color_Buffer;

   procedure Clear_Depth_Buffer (Object : Framebuffer; Value : Buffers.Depth) is
      Aliased_Value : aliased constant Buffers.Depth := Value;
   begin
      API.Clear_Named_Framebuffer_Depth (Object.Reference.GL_Id,
                                         Low_Level.Enums.Depth_Buffer, 0,
                                         Aliased_Value'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
   end Clear_Depth_Buffer;

   procedure Clear_Stencil_Buffer (Object : Framebuffer; Value : Buffers.Stencil_Index) is
      Aliased_Value : aliased constant Buffers.Stencil_Index := Value;
   begin
      API.Clear_Named_Framebuffer_Stencil (Object.Reference.GL_Id,
                                           Low_Level.Enums.Stencil, 0,
                                           Aliased_Value'Unchecked_Access);
      Raise_Exception_On_OpenGL_Error;
   end Clear_Stencil_Buffer;

   procedure Clear_Depth_And_Stencil_Buffer (Object : Framebuffer;
                                             Depth_Value   : Buffers.Depth;
                                             Stencil_Value : Buffers.Stencil_Index) is
   begin
      API.Clear_Named_Framebuffer_Depth_Stencil (Object.Reference.GL_Id,
                                                 Low_Level.Enums.Depth_Stencil, 0,
                                                 Depth_Value, Stencil_Value);
      Raise_Exception_On_OpenGL_Error;
   end Clear_Depth_And_Stencil_Buffer;

end GL.Objects.Framebuffers;
