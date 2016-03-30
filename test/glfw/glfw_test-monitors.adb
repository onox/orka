--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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
         Put (Value'Img);
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
      Put_Line ("Position: (" & Param1'Img & "," & Param2'Img & ")");
      M.Get_Physical_Size (Param1, Param2);
      Put_Line ("Dimensions: " & Param1'Img & " x " & Param2'Img);

      Put_Line ("Video modes: ");
      for Mode of VM_List loop
         Put ("  [");
         if Mode = M.Current_Video_Mode then
            Put ("x");
         else
            Put (" ");
         end if;
         Put ("] dim(" & Mode.Width'Img & " x" & Mode.Height'Img);
         Put ("), rgb(" & Mode.Red_Bits'Img & "," &
                Mode.Green_Bits'Img & "," & Mode.Blue_Bits'Img);
         Put_Line ("), refresh(" & Mode.Refresh_Rate'Img & ")");
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
