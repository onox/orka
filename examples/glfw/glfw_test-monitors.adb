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

with Glfw.Monitors;
with Ada.Text_IO;

procedure Glfw_Test.Monitors is

   procedure Print_Gamma_Value_Array (Values : Glfw.Monitors.Gamma_Value_Array) is
      use Ada.Text_IO;
      First : Boolean := True;
   begin
      Put ("[");
      for Value of Values loop
         if First then
            First := False;
         else
            Put (",");
         end if;
         Put (Value'Image);
      end loop;
      Put_Line ("]");
   end Print_Gamma_Value_Array;

   procedure Print_Monitor_Info (M : Glfw.Monitors.Monitor) is
      use Ada.Text_IO;
      use type Glfw.Monitors.Video_Mode;

      Param1, Param2 : Integer;
      VM_List : constant Glfw.Monitors.Video_Mode_List := M.Video_Modes;
      Gamma_Ramp : constant Glfw.Monitors.Gamma_Ramp := M.Current_Gamma_Ramp;
   begin
      Put_Line ("Monitor """ & M.Name & """");
      M.Get_Position (Param1, Param2);
      Put_Line ("Position: (" & Param1'Image & "," & Param2'Image & ")");
      M.Get_Physical_Size (Param1, Param2);
      Put_Line ("Dimensions: " & Param1'Image & " x " & Param2'Image);

      Put_Line ("Video modes: ");
      for Mode of VM_List loop
         Put ("  [");
         if Mode = M.Current_Video_Mode then
            Put ("x");
         else
            Put (" ");
         end if;
         Put ("] dim(" & Mode.Width'Image & " x" & Mode.Height'Image);
         Put ("), rgb(" & Mode.Red_Bits'Image & "," &
                Mode.Green_Bits'Image & "," & Mode.Blue_Bits'Image);
         Put_Line ("), refresh(" & Mode.Refresh_Rate'Image & ")");
      end loop;

      Put_Line ("Gamma ramp:");
      Put ("  red:   ");
      Print_Gamma_Value_Array (Gamma_Ramp.Red);
      Put ("  green: ");
      Print_Gamma_Value_Array (Gamma_Ramp.Green);
      Put ("  blue:  ");
      Print_Gamma_Value_Array (Gamma_Ramp.Blue);
   end Print_Monitor_Info;

begin
   Glfw.Init;
   Enable_Print_Errors;

   for Monitor of Glfw.Monitors.Monitors loop
      Print_Monitor_Info (Monitor);
   end loop;

   Glfw.Shutdown;
end Glfw_Test.Monitors;
