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

with GL.API;
with GL.Enums;

with Interfaces.C.Strings;

package body GL.Objects.Programs is

   procedure Attach (Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Attach_Shader (Subject.Reference.GL_Id, Shader.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Attach;

   procedure Link (Subject : Program) is
   begin
      API.Link_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Link;

   function Link_Status (Subject : Program) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Link_Status,
                             Status_Value);
      Raise_Exception_On_OpenGL_Error;
      return Status_Value /= 0;
   end Link_Status;

   function Info_Log (Subject : Program) return String is
      Log_Length : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Info_Log_Length,
                             Log_Length);
      Raise_Exception_On_OpenGL_Error;
      -- Returned length includes null termination character
      Log_Length := Log_Length - 1;
      declare
         Info_Log : String (1 .. Integer (Log_Length));
         -- do not care that string does not get initialized
         pragma Warnings (Off, Info_Log);
         C_Info_Log : C.Strings.chars_ptr := C.Strings.New_String (Info_Log);
         Actual_Length : Size := 0;
      begin
         API.Get_Program_Info_Log (Subject.Reference.GL_Id,
                                   Log_Length + 1,
                                   Actual_Length, C_Info_Log);
         Raise_Exception_On_OpenGL_Error;
         Info_Log := C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
         C.Strings.Free (C_Info_Log);
         return Info_Log;
      end;
   end Info_Log;

   procedure Use_Program (Subject : Program) is
   begin
      API.Use_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Use_Program;

   procedure Set_Binary_Retrievable (Subject : Program; Retrievable : Boolean) is
   begin
      API.Program_Parameter_Bool (Subject.Reference.GL_Id,
                                  Enums.Program_Binary_Retrievable_Hint,
                                  Low_Level.Bool (Retrievable));
      Raise_Exception_On_OpenGL_Error;
   end Set_Binary_Retrievable;

   procedure Set_Separable (Subject : Program; Separable : Boolean) is
   begin
      API.Program_Parameter_Bool (Subject.Reference.GL_Id, Enums.Program_Separable,
                                  Low_Level.Bool (Separable));
      Raise_Exception_On_OpenGL_Error;
   end Set_Separable;

   procedure Set_Feedback_Outputs (Object : Program; Names : String_Array;
                                   Format : Transform_Feedbacks.Outputs_Format) is
      C_Source : Low_Level.CharPtr_Array (Names'Range);
   begin
      for Index in Names'Range loop
         C_Source (Index) := C.Strings.New_String (Names (Index).all);
      end loop;
      API.Transform_Feedback_Varyings (Object.Reference.GL_Id, Names'Length, C_Source, Format);
      for C_String of C_Source loop
         C.Strings.Free (C_String);
      end loop;
      Raise_Exception_On_OpenGL_Error;
   end Set_Feedback_Outputs;

   overriding
   procedure Initialize_Id (Object : in out Program) is
   begin
      Object.Reference.GL_Id := API.Create_Program;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Program) is
   begin
      API.Delete_Program (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform is
      Result : constant Uniforms.Uniform := API.Get_Uniform_Location
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Uniform_Location;
   
   procedure Bind_Attrib_Location (Subject : Program;
                                   Index : Attributes.Attribute;
                                   Name : String) is
   begin
      API.Bind_Attrib_Location (Subject.Reference.GL_Id, Index,
                               Interfaces.C.To_C (Name));
      Raise_Exception_On_OpenGL_Error;
   end Bind_Attrib_Location;

   function Attrib_Location (Subject : Program; Name : String)
     return Attributes.Attribute is
      Location : constant Attributes.Attribute := API.Get_Attrib_Location
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Location;
   end Attrib_Location;

   function Attached_Shaders (Object : Program) return Shaders.Lists.List is
      Shader_Count : aliased Int := 0;
   begin
      API.Get_Program_Param (Object.Reference.GL_Id, Enums.Attached_Shaders,
                             Shader_Count);
      Raise_Exception_On_OpenGL_Error;

      return List : constant Shaders.Lists.List := Shaders.Lists.Create
        (API.Get_Attached_Shaders (Object.Reference.GL_Id,
                                   Size (Shader_Count)))
      do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Attached_Shaders;

   -----------------------------------------------------------------------------
   --                               Subroutines                               --
   -----------------------------------------------------------------------------

   function Subroutine_Interface (Shader : Shaders.Shader_Type)
     return Enums.Program_Interface with Inline is
   begin
      case Shader is
         when Shaders.Vertex_Shader =>
            return Enums.Vertex_Subroutine;
         when Shaders.Geometry_Shader =>
            return Enums.Geometry_Subroutine;
         when Shaders.Fragment_Shader =>
            return Enums.Fragment_Subroutine;
         when Shaders.Tess_Control_Shader =>
            return Enums.Tess_Control_Subroutine;
         when Shaders.Tess_Evaluation_Shader =>
            return Enums.Tess_Evaluation_Subroutine;
         when Shaders.Compute_Shader =>
            return Enums.Compute_Subroutine;
      end case;
   end Subroutine_Interface;

   function Subroutine_Uniform_Interface (Shader : Shaders.Shader_Type)
     return Enums.Program_Interface with Inline is
   begin
      case Shader is
         when Shaders.Vertex_Shader =>
            return Enums.Vertex_Subroutine_Uniform;
         when Shaders.Geometry_Shader =>
            return Enums.Geometry_Subroutine_Uniform;
         when Shaders.Fragment_Shader =>
            return Enums.Fragment_Subroutine_Uniform;
         when Shaders.Tess_Control_Shader =>
            return Enums.Tess_Control_Subroutine_Uniform;
         when Shaders.Tess_Evaluation_Shader =>
            return Enums.Tess_Evaluation_Subroutine_Uniform;
         when Shaders.Compute_Shader =>
            return Enums.Compute_Subroutine_Uniform;
      end case;
   end Subroutine_Uniform_Interface;

   function Active_Subroutines (Object : Program; Shader : Shaders.Shader_Type)
                                return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutines, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutines;

   function Active_Subroutine_Uniforms (Object : Program;
                                        Shader : Shaders.Shader_Type)
                                        return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniforms, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Uniforms;

   function Active_Subroutine_Uniform_Locations (Object : Program;
                                                 Shader : Shaders.Shader_Type)
                                                 return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniform_Locations, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Uniform_Locations;
      
   function Active_Subroutine_Uniform_Max_Length (Object : Program;
                                                  Shader : Shaders.Shader_Type)
                                                  return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniform_Max_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      --  Received length includes null termination character
      return Ret - 1;
   end Active_Subroutine_Uniform_Max_Length;

   function Active_Subroutine_Max_Length (Object : Program;
                                          Shader : Shaders.Shader_Type)
                                          return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Max_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      --  Received length includes null termination character
      return Ret - 1;
   end Active_Subroutine_Max_Length;

   function Subroutine_Index (Object : Program; Shader : Shaders.Shader_Type;
                              Name   : String)
                              return Subroutine_Index_Type is
      C_String : constant Interfaces.C.char_array
        := Interfaces.C.To_C (Name);
   begin
      return Index : constant Subroutine_Index_Type := Subroutine_Index_Type
        (API.Get_Program_Resource_Index (Object.Reference.GL_Id,
                                         Subroutine_Interface (Shader), C_String))
      do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Subroutine_Index;

   function Subroutine_Uniform_Location (Object : Program;
                                         Shader : Shaders.Shader_Type;
                                         Name   : String)
                                         return Uniform_Location_Type is
      C_String : constant Interfaces.C.char_array
        := Interfaces.C.To_C (Name);
   begin
      return Index : constant Uniform_Location_Type
        := Uniform_Location_Type (API.Get_Program_Resource_Location
             (Object.Reference.GL_Id,
              Subroutine_Uniform_Interface (Shader),
              C_String))
      do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Subroutine_Uniform_Location;

   function Subroutine_Name (Object : Program; Shader : Shaders.Shader_Type;
                             Index  : Subroutine_Index_Type)
     return String is
      Values : constant Int_Array := API.Get_Program_Resource
        (Object.Reference.GL_Id,
         Subroutine_Interface (Shader), Index,
         1, (1 => Enums.Name_Length), 1);
      --  Received length includes null termination character
      Max_Length : constant Size := Size (Values (1) - 1);
   begin
      Raise_Exception_On_OpenGL_Error;
      declare
         Name : String (1 .. Integer (Max_Length));
         pragma Warnings (Off, Name);
         C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
         Actual_Length : Size := 0;
      begin
         API.Get_Program_Resource_Name (Object.Reference.GL_Id,
                                        Subroutine_Interface (Shader), Index,
                                        Max_Length + 1, Actual_Length, C_Name);
         Raise_Exception_On_OpenGL_Error;
         Name := C.Strings.Value (C_Name, C.size_t (Actual_Length));
         C.Strings.Free (C_Name);
         return Name;
      end;
   end Subroutine_Name;

   function Subroutine_Uniform_Name (Object : Program; Shader : Shaders.Shader_Type;
                                     Index  : Subroutine_Index_Type)
     return String is
      Values : constant Int_Array := API.Get_Program_Resource
        (Object.Reference.GL_Id,
         Subroutine_Uniform_Interface (Shader), Index,
         1, (1 => Enums.Name_Length), 1);
      --  Received length includes null termination character
      Max_Length : constant Size := Size (Values (1) - 1);
   begin
      declare
         Name : String (1 .. Integer (Max_Length));
         pragma Warnings (Off, Name);
         C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
         Actual_Length : Size := 0;
      begin
         API.Get_Program_Resource_Name (Object.Reference.GL_Id,
                                        Subroutine_Uniform_Interface (Shader),
                                        Index, Max_Length + 1, Actual_Length, C_Name);
         Raise_Exception_On_OpenGL_Error;
         Name := C.Strings.Value (C_Name, C.size_t (Actual_Length));
         C.Strings.Free (C_Name);
         return Name;
      end;
   end Subroutine_Uniform_Name;

   procedure Set_Uniform_Subroutines (Shader : Shaders.Shader_Type; Indices : UInt_Array) is
   begin
      API.Uniform_Subroutines (Shader, Indices'Length, Indices);
      Raise_Exception_On_OpenGL_Error;
   end Set_Uniform_Subroutines;

   function Uniform_Subroutine (Shader : Shaders.Shader_Type; Location : Uniform_Location_Type)
     return Subroutine_Index_Type is
      Value : Subroutine_Index_Type := Invalid_Index;
   begin
      API.Get_Uniform_Subroutine (Shader, Location, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Uniform_Subroutine;

end GL.Objects.Programs;
