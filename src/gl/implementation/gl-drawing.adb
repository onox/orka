--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with GL.API;

package body GL.Drawing is

   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count : Size) is
   begin
      API.Draw_Arrays (Mode, Offset, Count);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays;

   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count, Instances : Size) is
   begin
      API.Draw_Arrays_Instanced (Mode, Offset, Count, Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays;

   procedure Draw_Arrays_Base_Instance (Mode : Connection_Mode; Offset, Count : Size;
                                        Instances, Base_Instance : Size) is
   begin
      API.Draw_Arrays_Instanced_Base_Instance (Mode, Offset, Count, Instances, UInt (Base_Instance));
      Raise_Exception_On_OpenGL_Error;
   end Draw_Arrays_Base_Instance;

   procedure Draw_Multiple_Arrays (Mode : Connection_Mode;
                                   Offsets, Counts : Size_Array) is
   begin
      API.Multi_Draw_Arrays (Mode, Offsets, Counts, Counts'Length);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Multiple_Arrays;

   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type) is
   begin
      API.Draw_Elements (Mode, Count, Index_Type, 0);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements;

   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Instances  : Types.Size) is
   begin
      API.Draw_Elements_Instanced (Mode, Count, Index_Type, 0, Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements;

   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Vertex_Offset, Index_Offset : Int) is
   begin
      API.Draw_Elements_Base_Vertex (Mode, Count, Index_Type, Index_Offset, Vertex_Offset);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements_Base_Vertex;

   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Instances  : Types.Size;
                            Vertex_Offset, Index_Offset : Int) is
   begin
      API.Draw_Elements_Instanced_Base_Vertex (Mode, Count, Index_Type, Index_Offset,
                                               Instances, Vertex_Offset);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements_Base_Vertex;

   procedure Draw_Elements_Base_Vertex_Base_Instance (Mode : Connection_Mode; Count : Size;
                                                      Index_Type : Unsigned_Numeric_Type;
                                                      Instances  : Size;
                                                      Vertex_Offset, Index_Offset : Int;
                                                      Base_Instance : Size) is
   begin
      API.Draw_Elements_Instanced_Base_Vertex_Base_Instance
        (Mode, Count, Index_Type, Index_Offset,
         Instances, Vertex_Offset, UInt (Base_Instance));
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements_Base_Vertex_Base_Instance;

   procedure Draw_Elements_Base_Instance (Mode : Connection_Mode; Count : Size;
                                          Index_Type : Unsigned_Numeric_Type;
                                          Instances  : Size;
                                          Index_Offset : Int;
                                          Base_Instance : Size) is
   begin
      API.Draw_Elements_Instanced_Base_Instance (Mode, Count, Index_Type, Index_Offset,
                                                 Instances, UInt (Base_Instance));
      Raise_Exception_On_OpenGL_Error;
   end Draw_Elements_Base_Instance;

   procedure Draw_Multiple_Elements (Mode : Connection_Mode;
                                     Index_Type : Unsigned_Numeric_Type;
                                     Counts, Index_Offsets : Size_Array) is
   begin
      API.Multi_Draw_Elements (Mode, Counts, Index_Type, Index_Offsets, Counts'Length);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Multiple_Elements;

   procedure Draw_Multiple_Elements_Base_Vertex (Mode : Connection_Mode;
                                                 Index_Type : Unsigned_Numeric_Type;
                                                 Counts, Vertex_Offsets, Index_Offsets : Size_Array) is
   begin
      API.Multi_Draw_Elements_Base_Vertex (Mode, Counts, Index_Type, Index_Offsets,
                                           Counts'Length, Vertex_Offsets);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Multiple_Elements_Base_Vertex;

   procedure Draw_Multiple_Elements_Indirect (Mode : Connection_Mode;
                                              Index_Type : Unsigned_Numeric_Type;
                                              Count : Types.Size) is
   begin
      API.Multi_Draw_Elements_Indirect (Mode, Index_Type, 0, Count, 0);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Multiple_Elements_Indirect;

end GL.Drawing;
