--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with Ada.Containers.Indefinite_Holders;

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Renderbuffers is

   package Renderbuffer_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Renderbuffer'Class);

   type Renderbuffer_Target_Array is array (Low_Level.Enums.Renderbuffer_Kind)
     of Renderbuffer_Holder.Holder;
   Current_Renderbuffers : Renderbuffer_Target_Array;

   procedure Allocate (Object : Renderbuffer;
                       Format : Pixels.Internal_Format;
                       Width, Height : Size;
                       Samples : Size := 0) is
   begin
      API.Named_Renderbuffer_Storage_Multisample
        (Object.Reference.GL_Id, Samples, Format, Width, Height);
      Raise_Exception_On_OpenGL_Error;
   end Allocate;

   function Width (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Width, Value);
      return Value;
   end Width;

   function Height (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Height, Value);
      return Value;
   end Height;

   function Internal_Format (Object : Renderbuffer)
                             return Pixels.Internal_Format is
      Value : Pixels.Internal_Format := Pixels.Internal_Format'First;
   begin
      API.Get_Named_Renderbuffer_Parameter_Internal_Format
        (Object.Reference.GL_Id, Enums.Getter.Internal_Format, Value);
      return Value;
   end Internal_Format;

   function Red_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Green_Size, Value);
      return Value;
   end Red_Size;

   function Green_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Green_Size, Value);
      return Value;
   end Green_Size;

   function Blue_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Blue_Size, Value);
      return Value;
   end Blue_Size;

   function Alpha_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Alpha_Size, Value);
      return Value;
   end Alpha_Size;

   function Depth_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Depth_Size, Value);
      return Value;
   end Depth_Size;

   function Stencil_Size (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Stencil_Size, Value);
      return Value;
   end Stencil_Size;

   function Samples (Object : Renderbuffer) return Size is
      Value : Int := 0;
   begin
      API.Get_Named_Renderbuffer_Parameter_Int
        (Object.Reference.GL_Id, Enums.Getter.Samples, Value);
      return Value;
   end Samples;

   function Raw_Kind (Object : Renderbuffer_Target)
                      return Low_Level.Enums.Renderbuffer_Kind is
   begin
      return Object.Kind;
   end Raw_Kind;

   procedure Bind (Target : Renderbuffer_Target; Object : Renderbuffer'Class) is
      Holder : Renderbuffer_Holder.Holder := Current_Renderbuffers (Target.Kind);
   begin
      if Holder.Is_Empty or else Object /= Holder.Element then
         API.Bind_Renderbuffer (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         Holder.Replace_Element (Object);
      end if;
   end Bind;

   function Current (Target : Renderbuffer_Target) return Renderbuffer'Class is
      Holder : constant Renderbuffer_Holder.Holder := Current_Renderbuffers (Target.Kind);
   begin
      if Holder.Is_Empty then
         raise No_Object_Bound_Exception with GL.Low_Level.Enums.Renderbuffer_Kind'Image (Target.Kind);
      else
         return Holder.Element;
      end if;
   end Current;

   overriding
   procedure Initialize_Id (Object : in out Renderbuffer) is
      New_Id : UInt := 0;
   begin
      API.Create_Renderbuffers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Renderbuffer) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Renderbuffers (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

end GL.Objects.Renderbuffers;
