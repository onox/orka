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

with Interfaces.C.Strings;

with GL.API;
with GL.Enums;

package body GL.Objects.Shaders is

   procedure Set_Source (Subject : Shader; Source : String) is
      C_Shader_Source : C.Strings.chars_ptr := C.Strings.New_String (Source);
      C_Source : constant Low_Level.CharPtr_Array
        := (1 => C_Shader_Source);
      Lengths : constant Low_Level.Int_Array
        := (1 => Source'Length);
   begin
      API.Shader_Source (Subject.Reference.GL_Id, 1, C_Source, Lengths);
      C.Strings.Free (C_Shader_Source);
      Raise_Exception_On_OpenGL_Error;
   end Set_Source;

   function Source (Subject : Shader) return String is
      Source_Length : Size := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Shader_Source_Length, Source_Length);
      Raise_Exception_On_OpenGL_Error;

      if Source_Length = 0 then
         return "";
      end if;

      declare
         Shader_Source : String (1 .. Integer (Source_Length));
      begin
         API.Get_Shader_Source (Subject.Reference.GL_Id, Source_Length,
                                Source_Length, Shader_Source);
         Raise_Exception_On_OpenGL_Error;
         return Shader_Source (1 .. Integer (Source_Length));
      end;
   end Source;

   procedure Compile (Subject : Shader) is
   begin
      API.Compile_Shader (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Compile;

   procedure Release_Shader_Compiler renames API.Release_Shader_Compiler;

   function Compile_Status (Subject : Shader) return Boolean is
      Value : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id, Enums.Compile_Status,
                            Value);
      return Value /= 0;
   end Compile_Status;

   function Info_Log (Subject : Shader) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Info_Log_Length, Log_Length);
      Raise_Exception_On_OpenGL_Error;

      if Log_Length = 0 then
         return "";
      end if;

      declare
         Info_Log : String (1 .. Integer (Log_Length));
      begin
         API.Get_Shader_Info_Log (Subject.Reference.GL_Id, Log_Length,
                                  Log_Length, Info_Log);
         Raise_Exception_On_OpenGL_Error;
         return Info_Log (1 .. Integer (Log_Length));
      end;
   end Info_Log;

   overriding
   procedure Initialize_Id (Object : in out Shader) is
   begin
      Object.Reference.GL_Id := API.Create_Shader (Object.Kind);
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Shader) is
   begin
      API.Delete_Shader (Object.Reference.GL_Id);
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   function Create_From_Id (Id : UInt) return Shader is
      Kind : Shader_Type := Shader_Type'First;
   begin
      API.Get_Shader_Type (Id, Enums.Shader_Type, Kind);
      Raise_Exception_On_OpenGL_Error;
      return Object : Shader (Kind) do
         Object.Reference.GL_Id := Id;
         Object.Reference.Initialized := True;
      end return;
   end Create_From_Id;

end GL.Objects.Shaders;
