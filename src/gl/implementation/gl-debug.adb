--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with System;

with Interfaces.C.Strings;

with Ada.Unchecked_Deallocation;

with GL.API;
with GL.Enums.Getter;

package body GL.Debug is

   Any : constant Low_Level.Enum := 16#1100#;

   Current_Callback : Callback_Reference := null;

   procedure Debug_Callback
     (From      : Source;
      Kind      : Message_Type;
      ID        : GL.Types.UInt;
      Level     : Severity;
      Length    : GL.Types.Size;
      C_Message : C.Strings.chars_ptr;
      User_Data : System.Address)
   with Convention => StdCall;

   procedure Debug_Callback
     (From      : Source;
      Kind      : Message_Type;
      ID        : GL.Types.UInt;
      Level     : Severity;
      Length    : GL.Types.Size;
      C_Message : C.Strings.chars_ptr;
      User_Data : System.Address)
   is
      Message : constant String := C.Strings.Value (C_Message, C.size_t (Length));
   begin
      if Current_Callback /= null then
         Current_Callback (From, Kind, Level, ID, Message);
      end if;
   end Debug_Callback;

   procedure Set_Message_Callback (Callback : not null Callback_Reference) is
   begin
      API.Debug_Message_Callback (Debug_Callback'Address, System.Null_Address);
      Raise_Exception_On_OpenGL_Error;
      Current_Callback := Callback;
   end Set_Message_Callback;

   procedure Disable_Message_Callback is
   begin
      API.Debug_Message_Callback (System.Null_Address, System.Null_Address);
      Raise_Exception_On_OpenGL_Error;
      Current_Callback := null;
   end Disable_Message_Callback;

   procedure Set (From : Source; Kind : Message_Type; Level : Severity;
                  Enabled : Boolean) is
      Identifiers : Types.UInt_Array (1 .. 0);
   begin
      API.Debug_Message_Control
        (From, Kind, Level, 0, Identifiers, Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Set;

   procedure Set (Level : Severity; Enabled : Boolean) is
      Identifiers : Types.UInt_Array (1 .. 0);
   begin
      API.Debug_Message_Control
        (Any, Any, Level, 0, Identifiers, Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Set;

   procedure Set (From : Source; Kind : Message_Type; Identifiers : Types.UInt_Array;
                  Enabled : Boolean) is
   begin
      API.Debug_Message_Control
        (From, Kind, Any, Types.Size (Identifiers'Length),
         Identifiers, Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Set;

   procedure Insert_Message (From : Source; Kind : Message_Type; Level : Severity;
                             Identifier : UInt; Message : String) is
      pragma Assert (From in Third_Party | Application);
   begin
      API.Debug_Message_Insert (From, Kind, Identifier, Level,
                                Types.Size (Message'Length), C.To_C (Message));
      Raise_Exception_On_OpenGL_Error;
   end Insert_Message;

   function Push_Debug_Group (From : Source; Identifier : UInt; Message : String)
     return Active_Group'Class is
      pragma Assert (From in Third_Party | Application);
   begin
      API.Push_Debug_Group (From, Identifier,
                            Types.Size (Message'Length), C.To_C (Message));
      Raise_Exception_On_OpenGL_Error;
      return Active_Group'(Ada.Finalization.Limited_Controlled with Finalized => False);
   end Push_Debug_Group;

   overriding
   procedure Finalize (Object : in out Active_Group) is
   begin
      if not Object.Finalized then
         API.Pop_Debug_Group;
         Object.Finalized := True;
      end if;
   end Finalize;

   procedure Annotate (Object : GL.Objects.GL_Object'Class; Message : String) is
   begin
      API.Object_Label (Object.Identifier, Object.Raw_Id, Types.Size (Message'Length), C.To_C (Message));
      Raise_Exception_On_OpenGL_Error;
   end Annotate;

   function Get_Label (Object : GL.Objects.GL_Object'Class) return String is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Types.Size, Name => GL.Low_Level.Size_Access);

      C_Size : GL.Low_Level.Size_Access := new Types.Size'(0);
      Label_Size : Types.Size := 0;
   begin
      API.Get_Object_Label_Length
        (Object.Identifier, Object.Raw_Id, 0,
         C_Size, Interfaces.C.Strings.Null_Ptr);

      --  Deallocate before potentially raising an exception
      Label_Size := C_Size.all;
      Free (C_Size);

      Raise_Exception_On_OpenGL_Error;

      if Label_Size = 0 then
         return "";
      end if;

      declare
         Label : String (1 .. Integer (Label_Size));
      begin
         API.Get_Object_Label (Object.Identifier, Object.Raw_Id, Label_Size,
                               Label_Size, Label);
         Raise_Exception_On_OpenGL_Error;
         return Label;
      end;
   end Get_Label;

   function Max_Message_Length return Size is
      Result : Int := 0;
   begin
      API.Get_Integer (Enums.Getter.Max_Debug_Message_Length, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Max_Message_Length;

   package body Messages is
      Identifier : GL.Types.UInt := 0;

      procedure Insert (Level : Severity; Message : String) is
      begin
         GL.Debug.Insert_Message (From, Kind, Level, Identifier, Message);
         Identifier := Identifier + 1;
      end Insert;

      procedure Reset_Identifier (Value : UInt) is
      begin
         Identifier := Value;
      end Reset_Identifier;
   end Messages;

end GL.Debug;
