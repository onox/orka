--  SPDX-License-Identifier: Apache-2.0
--
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

with Interfaces.C.Strings;

with Ada.Task_Identification;

with Glfw.API;

package body Glfw.Input.Joysticks is

   function Index (Source : Joystick) return Joystick_Index is
     (Enums.Joystick_ID'Pos (Source.Raw_Index) + 1);

   function Get_Joystick (Index : Joystick_Index) return Joystick is
     ((Raw_Index => Enums.Joystick_ID'Val (Index - 1)));

   function Present (Source : Joystick) return Boolean is
     (Boolean (API.Joystick_Present (Source.Raw_Index)));

   function Is_Gamepad (Source : Joystick) return Boolean is
     (Boolean (API.Joystick_Is_Gamepad (Source.Raw_Index)));

   function Gamepad_Name (Source : Joystick) return String is
     (Interfaces.C.Strings.Value (API.Get_Gamepad_Name (Source.Raw_Index)));

   function Gamepad_State (Source : Joystick) return Joystick_Gamepad_State is
      State  : Joystick_Gamepad_State;
      Result : constant Bool := API.Get_Gamepad_State (Source.Raw_Index, State);
   begin
      if Result then
         return State;
      else
         raise Operation_Exception;
      end if;
   end Gamepad_State;

   function Joystick_Name (Source : Joystick) return String is
     (Interfaces.C.Strings.Value (API.Get_Joystick_Name (Source.Raw_Index)));

   function Joystick_GUID (Source : Joystick) return String is
     (Interfaces.C.Strings.Value (API.Get_Joystick_GUID (Source.Raw_Index)));

   procedure Update_Gamepad_Mappings (Mappings : String) is
      C_Mappings : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (Mappings);

      Result : constant Bool := API.Update_Gamepad_Mappings (C_Mappings);
   begin
      Interfaces.C.Strings.Free (C_Mappings);
      if not Result then
         raise Operation_Exception;
      end if;
   end Update_Gamepad_Mappings;
   
   function Positions (Source : Joystick) return Axis_Positions is
      Count : aliased Interfaces.C.int;
      Raw : constant API.Axis_Position_List_Pointers.Pointer
        := API.Get_Joystick_Axes (Source.Raw_Index, Count'Access);
   begin
      return API.Axis_Position_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Positions;

   function Button_States (Source : Joystick) return Joystick_Button_States is
      Count : aliased Interfaces.C.int;
      Raw : constant API.Joystick_Button_State_List_Pointers.Pointer
        := API.Get_Joystick_Buttons (Source.Raw_Index, Count'Access);
   begin
      return API.Joystick_Button_State_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Button_States;

   function Hat_States (Source : Joystick) return Joystick_Hat_States is
      Count : Interfaces.C.int;
      Raw : constant API.Joystick_Hat_State_List_Pointers.Pointer
        := API.Get_Joystick_Hats (Source.Raw_Index, Count);
   begin
      return API.Joystick_Hat_State_List_Pointers.Value
        (Raw, Interfaces.C.ptrdiff_t (Count));
   end Hat_States;

   Current_Callback : Joystick_Callback := null;

   procedure Raw_Handler (ID : Enums.Joystick_ID; Event : Connect_State)
     with Convention => C;

   procedure Raw_Handler (ID : Enums.Joystick_ID; Event : Connect_State) is
   begin
      if Current_Callback /= null then
         Current_Callback.all ((Raw_Index => ID), Event);
      end if;
   end Raw_Handler;

   use Ada.Task_Identification;

   procedure Set_Callback (Callback : Joystick_Callback) is
      pragma Assert (Current_Task = Environment_Task);
   begin
      Current_Callback := Callback;
      if Callback = null then
         API.Set_Joystick_Callback (null);
      else
         API.Set_Joystick_Callback (Raw_Handler'Access);
      end if;
   end Set_Callback;

end Glfw.Input.Joysticks;
