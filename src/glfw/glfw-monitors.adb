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

with System.Address_To_Access_Conversions;

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Monitors is

   package Conversions is new System.Address_To_Access_Conversions (Monitor'Class);

   function Monitor_Ptr (Raw : System.Address) return not null access Monitor'Class is
   begin
      return Conversions.To_Pointer (API.Get_Monitor_User_Pointer (Raw));
   end Monitor_Ptr;

   function No_Monitor return Monitor is
     ((Handle => System.Null_Address));

   function "=" (Left, Right : Monitor) return Boolean is
      use type System.Address;
   begin
      return Left.Handle = Right.Handle;
   end "=";

   function Monitors return Monitor_List is
      use type API.Address_List_Pointers.Pointer;
      Count : aliased Interfaces.C.int;
      Raw : constant API.Address_List_Pointers.Pointer :=
        API.Get_Monitors (Count'Access);
   begin
      if Raw /= null then
         declare
            List : constant API.Address_List := API.Address_List_Pointers.Value
              (Raw, Interfaces.C.ptrdiff_t (Count));
         begin
            return Ret : Monitor_List (List'Range) do
               for I in List'Range loop
                  Ret (I).Handle := List (I);
                  API.Set_Monitor_User_Pointer (Ret (I).Handle, Conversions.To_Address
                    (Conversions.Object_Pointer'(Ret (I)'Unchecked_Access)));
               end loop;
            end return;
         end;
      else
         raise Operation_Exception;
      end if;
   end Monitors;

   function To_Monitor (Raw : System.Address) return Monitor is
      use type System.Address;
   begin
      if Raw /= System.Null_Address then
         return Result : Monitor := Monitor'(Handle => Raw) do
            API.Set_Monitor_User_Pointer (Result.Handle, Conversions.To_Address
                 (Conversions.Object_Pointer'(Result'Unchecked_Access)));
         end return;
      else
         raise Operation_Exception;
      end if;
   end To_Monitor;

   function Primary_Monitor return Monitor is (To_Monitor (API.Get_Primary_Monitor));

   procedure Get_Position (Object : Monitor; X, Y : out Integer) is
      X_Raw, Y_Raw : Interfaces.C.int;
   begin
      API.Get_Monitor_Pos (Object.Handle, X_Raw, Y_Raw);
      X := Integer (X_Raw);
      Y := Integer (Y_Raw);
   end Get_Position;

   procedure Get_Physical_Size (Object : Monitor;
                                Width, Height : out Integer) is
      Width_Raw, Height_Raw : Interfaces.C.int;
   begin
      API.Get_Monitor_Physical_Size (Object.Handle, Width_Raw, Height_Raw);
      Width  := Integer (Width_Raw);
      Height := Integer (Height_Raw);
   end Get_Physical_Size;

   procedure Get_Content_Scale (Object : Monitor; X, Y : out Float) is
      X_Raw, Y_Raw : Interfaces.C.C_float;
   begin
      API.Get_Monitor_Content_Scale (Object.Handle, X_Raw, Y_Raw);
      X := Float (X_Raw);
      Y := Float (Y_Raw);
   end Get_Content_Scale;

   procedure Get_Workarea (Object : Monitor; X, Y, Width, Height : out Integer) is
      X_Raw, Y_Raw, Width_Raw, Height_Raw : Interfaces.C.int;
   begin
      API.Get_Monitor_Workarea (Object.Handle, X_Raw, Y_Raw, Width_Raw, Height_Raw);
      X := Integer (X_Raw);
      Y := Integer (Y_Raw);
      Width  := Integer (Width_Raw);
      Height := Integer (Height_Raw);
   end Get_Workarea;

   function Name (Object : Monitor) return String is
   begin
      return Interfaces.C.Strings.Value (API.Get_Monitor_Name (Object.Handle));
   end Name;

   function Video_Modes (Object : Monitor) return Video_Mode_List is
      use type API.VMode_List_Pointers.Pointer;
      Count : aliased Interfaces.C.int;
      Raw   : constant API.VMode_List_Pointers.Pointer
        := API.Get_Video_Modes (Object.Handle, Count'Access);
   begin
      if Raw /= null then
         return API.VMode_List_Pointers.Value (Raw,
                                               Interfaces.C.ptrdiff_t (Count));
      else
         raise Operation_Exception;
      end if;
   end Video_Modes;

   function Current_Video_Mode (Object : Monitor) return Video_Mode is
   begin
      return API.Get_Video_Mode (Object.Handle).all;
   end Current_Video_Mode;

   procedure Set_Gamma (Object : Monitor; Gamma : Float) is
   begin
      API.Set_Gamma (Object.Handle, Interfaces.C.C_float (Gamma));
   end Set_Gamma;

   function Current_Gamma_Ramp (Object : Monitor) return Gamma_Ramp is
      Raw : constant access constant API.Raw_Gamma_Ramp
        := API.Get_Gamma_Ramp (Object.Handle);
   begin
      return Ret : Gamma_Ramp (Integer (Raw.Size)) do
         Ret.Red := API.Unsigned_Short_List_Pointers.Value
           (Raw.Red, Interfaces.C.ptrdiff_t (Raw.Size));
         Ret.Green := API.Unsigned_Short_List_Pointers.Value
           (Raw.Green, Interfaces.C.ptrdiff_t (Raw.Size));
         Ret.Blue := API.Unsigned_Short_List_Pointers.Value
           (Raw.Blue, Interfaces.C.ptrdiff_t (Raw.Size));
      end return;
   end Current_Gamma_Ramp;

   procedure Set_Gamma_Ramp (Object : Monitor; Value : Gamma_Ramp) is
      Raw : aliased API.Raw_Gamma_Ramp;
      Ramp : Gamma_Ramp := Value;
   begin
      Raw.Size  := Interfaces.C.unsigned (Ramp.Size);
      Raw.Red   := Ramp.Red   (Ramp.Red'First)'Unchecked_Access;
      Raw.Green := Ramp.Green (Ramp.Green'First)'Unchecked_Access;
      Raw.Blue  := Ramp.Blue  (Ramp.Blue'First)'Unchecked_Access;

      API.Set_Gamma_Ramp (Object.Handle, Raw'Access);
   end Set_Gamma_Ramp;

   function Raw_Pointer (Object : Monitor) return System.Address is
   begin
      return Object.Handle;
   end Raw_Pointer;

   procedure Raw_Handler (Monitor : System.Address; State : Event)
     with Convention => C;

   procedure Raw_Handler (Monitor : System.Address; State : Event) is
   begin
      Monitor_Ptr (Monitor).Event_Occurred (State);
   end Raw_Handler;

   procedure Set_Callback (Object : Monitor; Enable : Boolean) is
   begin
      API.Set_Monitor_Callback
        (Object.Handle, (if Enable then Raw_Handler'Access else null));
   end Set_Callback;

end Glfw.Monitors;
