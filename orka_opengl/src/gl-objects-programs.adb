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

with Interfaces.C.Strings;

with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums;
with GL.Low_Level;
with GL.Objects.Programs.Uniforms;

with Orka;

package body GL.Objects.Programs is

   procedure Attach (Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Attach_Shader.Ref (Subject.Reference.GL_Id, Shader.Raw_Id);
   end Attach;

   procedure Detach (Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Detach_Shader.Ref (Subject.Reference.GL_Id, Shader.Raw_Id);
   end Detach;

   procedure Link (Subject : Program) is
   begin
      API.Link_Program.Ref (Subject.Reference.GL_Id);
   end Link;

   function Link_Status (Subject : Program) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Get_Program_Param.Ref
        (Subject.Reference.GL_Id, Enums.Link_Status, Status_Value);
      return Status_Value /= 0;
   end Link_Status;

   function Info_Log (Subject : Program) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Program_Param.Ref
        (Subject.Reference.GL_Id, Enums.Info_Log_Length, Log_Length);

      if Log_Length = 0 then
         return "";
      end if;

      declare
         Info_Log : String (1 .. Integer (Log_Length));
      begin
         API.Get_Program_Info_Log.Ref
           (Subject.Reference.GL_Id, Log_Length, Log_Length, Info_Log);
         return Info_Log (1 .. Integer (Log_Length));
      end;
   end Info_Log;

   procedure Use_Program (Subject : Program) is
   begin
      API.Use_Program.Ref (Subject.Reference.GL_Id);
   end Use_Program;

   procedure Set_Separable (Subject : Program; Separable : Boolean) is
   begin
      API.Program_Parameter_Bool.Ref (Subject.Reference.GL_Id, Enums.Program_Separable,
                                  Low_Level.Bool (Separable));
   end Set_Separable;

   function Separable (Subject : Program) return Boolean is
      Separable_Value : Int := 0;
   begin
      API.Get_Program_Param.Ref
        (Subject.Reference.GL_Id, Enums.Program_Separable, Separable_Value);
      return Separable_Value /= 0;
   end Separable;

   function Compute_Work_Group_Size (Object : Program) return Compute.Dimension_Size_Array is
      Values : Compute.Dimension_Size_Array := [others => 0];
   begin
      API.Get_Program_Param_Compute.Ref
        (Object.Reference.GL_Id, Enums.Compute_Work_Group_Size, Values);
      return Values;
   end Compute_Work_Group_Size;

   overriding
   procedure Initialize_Id (Object : in out Program) is
   begin
      Object.Reference.GL_Id := API.Create_Program.Ref.all;
   end Initialize_Id;

   procedure Initialize_Id
     (Object : in out Program;
      Kind   : Shaders.Shader_Type;
      Source : String)
   is
      C_Shader_Source : C.Strings.chars_ptr := C.Strings.New_String (Source);
      C_Source : constant Low_Level.CharPtr_Array := [C_Shader_Source];
   begin
      Object.Reference.GL_Id := API.Create_Shader_Program.Ref (Kind, 1, C_Source);
      C.Strings.Free (C_Shader_Source);
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Program) is
   begin
      API.Delete_Program.Ref (Object.Reference.GL_Id);
      Object.Reference.GL_Id := 0;
   end Delete_Id;

   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform is
      Result : constant Int := API.Get_Uniform_Location.Ref
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      if Result = -1 then
         raise Uniform_Inactive_Error with "Uniform " & Name & " is inactive (unused)";
      end if;
      return Uniforms.Create_Uniform (Subject, Result);
   end Uniform_Location;

   function Buffer_Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Size
   is
      Index : UInt;
      Iface : Enums.Program_Interface;

      use all type Buffers.Indexed_Buffer_Target;
   begin
      case Target is
         when Shader_Storage =>
            Index := API.Get_Program_Resource_Index.Ref
              (Object.Reference.GL_Id, Enums.Shader_Storage_Block, Interfaces.C.To_C (Name));
            Iface := Enums.Shader_Storage_Block;
         when Uniform =>
            Index := API.Get_Program_Resource_Index.Ref
              (Object.Reference.GL_Id, Enums.Uniform_Block, Interfaces.C.To_C (Name));
            Iface := Enums.Uniform_Block;
      end case;

      if Index = -1 then
         raise Uniform_Inactive_Error with "Buffer " & Name & " is inactive (unused)";
      end if;

      declare
         Values : constant Orka.Integer_32_Array := API.Get_Program_Resource.Ref
           (Object.Reference.GL_Id, Iface, Index, 1, [Enums.Buffer_Binding], 1);
      begin
         return Size (Values (Values'First));
      end;
   end Buffer_Binding;

   function Uniform_Type (Object : Program; Name : String)
     return Low_Level.Enums.Resource_Type is
      Index : constant UInt := API.Get_Program_Resource_Index.Ref
        (Object.Reference.GL_Id, Enums.Uniform, Interfaces.C.To_C (Name));
   begin
      if Index = -1 then
         raise Uniform_Inactive_Error with "Uniform " & Name & " is inactive (unused)";
      end if;
      declare
         Values : constant Orka.Integer_32_Array := API.Get_Program_Resource.Ref
           (Object.Reference.GL_Id, Enums.Uniform, Index, 1, [Enums.Resource_Type], 1);

         function Convert is new Ada.Unchecked_Conversion
           (Source => Int, Target => Low_Level.Enums.Resource_Type);
      begin
         return Convert (Values (Values'First));
      end;
   end Uniform_Type;

   package body Internal is

      type No_Program_Type is new Program with null record;

      overriding procedure Initialize_Id (Object : in out No_Program_Type) is null;
      overriding procedure Delete_Id (Object : in out No_Program_Type) is null;

      Null_Program : constant No_Program_Type := No_Program_Type'(GL_Object with null record);

      function No_Program return Program is (Program (Null_Program));

   end Internal;

end GL.Objects.Programs;
