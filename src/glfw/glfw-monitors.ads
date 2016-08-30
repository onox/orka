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

with System;

package Glfw.Monitors is

   type Event is (Connected, Disconnected);

   type Video_Mode is record
      Width, Height : Interfaces.C.int;
      Red_Bits, Green_Bits, Blue_Bits : Interfaces.C.int;
      Refresh_Rate : Interfaces.C.int;
   end record;

   type Gamma_Value_Array is array (Positive range <>) of aliased
     Interfaces.C.unsigned_short;

   type Gamma_Ramp (Size : Positive) is record
      Red, Green, Blue : Gamma_Value_Array (1 .. Size);
   end record;

   type Monitor is tagged private;

   No_Monitor : constant Monitor;

   type Monitor_List is array (Positive range <>) of Monitor;
   type Video_Mode_List is array (Positive range <>) of aliased Video_Mode;

   pragma Convention (C, Video_Mode);
   pragma Convention (C, Video_Mode_List);

   function Monitors return Monitor_List;
   function Primary_Monitor return Monitor;

   procedure Get_Position (Object : Monitor; X, Y : out Integer);
   procedure Get_Physical_Size (Object : Monitor; Width, Height : out Integer);
   function Name (Object : Monitor) return String;
   function Video_Modes (Object : Monitor) return Video_Mode_List;
   function Current_Video_Mode (Object : Monitor) return Video_Mode;
   procedure Set_Gamma (Object : Monitor; Gamma : Float);
   function Current_Gamma_Ramp (Object : Monitor) return Gamma_Ramp;
   procedure Set_Gamma_Ramp (Object : Monitor; Value : Gamma_Ramp);

   -- used internally
   function Raw_Pointer (Object : Monitor) return System.Address;
private
   type Monitor is tagged record
      Handle : System.Address;
   end record;

   No_Monitor : constant Monitor := (Handle => System.Null_Address);

   for Event use (Connected    => 16#00040001#,
                  Disconnected => 16#00040002#);
   for Event'Size use Interfaces.C.int'Size;

end Glfw.Monitors;
