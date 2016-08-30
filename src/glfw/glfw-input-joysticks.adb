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

with Glfw.API;

package body Glfw.Input.Joysticks is

   function Index (Source : Joystick) return Joystick_Index is
   begin
      return Enums.Joystick_ID'Pos (Source.Raw_Index) + 1;
   end Index;
      
   procedure Set_Index (Target : in out Joystick; Value : Joystick_Index) is
   begin
      Target.Raw_Index := Enums.Joystick_ID'Val (Value - 1);
   end Set_Index;

   function Present (Source : Joystick) return Boolean is
   begin
      return Boolean (API.Joystick_Present (Source.Raw_Index));
   end Present;
   
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

end Glfw.Input.Joysticks;
