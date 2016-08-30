--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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
