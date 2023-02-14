--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Ada.Text_IO;

with GL.Barriers;

with Orka.Algorithms.FFT;
with Orka.Contexts.AWT;
with Orka.Logging;
with Orka.Rendering.Buffers;
with Orka.Resources.Locations.Directories;
with Orka.Types;
with Orka.Timers;

procedure Orka_6_FFT is
   Context : constant Orka.Contexts.Context'Class := Orka.Contexts.AWT.Create_Context
     (Version => (4, 2), Flags  => (Debug => True, others => False));

   ----------------------------------------------------------------------

   Count : constant := 2;
   --  Number of times to repeat the complex numbers

   In_Vertical_Direction : constant Boolean := False;
   --  Apply FFT in vertical direction instead of horizontal

   ----------------------------------------------------------------------

   use type Orka.Integer_32;
   use type Orka.Float_32;

   use Orka.Rendering.Buffers;
   use Orka.Resources;
   use Orka;

   use all type Orka.Types.Element_Type;

   Numbers_Small : constant Float_32_Array (1 .. 16) :=
     (-1.1594, -2.2122,
       0.6257, -0.4115,
       0.8245,  0.0956,
       0.1711, -0.8238,
      -0.6876,  0.0567,
      -2.4961,  0.7631,
       1.1099, -1.3213,
      -1.2707,  0.7561);

   Output_Numbers_Small : constant Float_32_Array (1 .. 16) :=
     (-2.882600, -3.097300,
       0.185311, -4.923875,
      -3.362100, -0.159000,
      -5.024379, -6.067858,
       3.057400, -3.665100,
       1.704890,  0.956875,
      -4.200700, -1.700600,
       1.246979,  0.959258);

   procedure Print_Array (Data : Float_32_Array) is
      function Image (Value : Float_32) return String is
         package Single_IO is new Ada.Text_IO.Float_IO (Float_32);

         Value_String : String := "123456789012.123456";
      begin
         Single_IO.Put (Value_String, Value, Aft => 6, Exp => 0);
         return Value_String;
      end Image;
   begin
      for Index in 1 .. Integer_32 (Data'Length / 2) loop
         Ada.Text_IO.Put_Line (Image (Data (Index * 2 - 1)) & " " & Image (Data (Index * 2)));
      end loop;
   end Print_Array;

   BO_1 : constant Buffer := Create_Buffer
     ((Dynamic_Storage => True, others => False), Single_Type, Numbers_Small'Length * Count);

   Timer_1 : Orka.Timers.Timer := Orka.Timers.Create_Timer;

   procedure Print_Numbers (Title : String) is
      Data : Float_32_Array (1 .. Size (BO_1.Length)) := (others => 0.0);
   begin
      BO_1.Get_Data (Data);

      Ada.Text_IO.Put_Line (Title &
        " (" & Orka.Logging.Trim (Integer'Image (Data'Length / 2)) & "):");
      Print_Array (Data);

      Timer_1.Wait_For_Result;

      Ada.Text_IO.Put_Line ("Duration " & Title & ": " & Orka.Logging.Image
        (Timer_1.GPU_Duration));
   end Print_Numbers;

   package Fourier_Transforms renames Orka.Algorithms.FFT;

   Location_Shaders : constant Locations.Location_Ptr
     := Locations.Directories.Create_Location ("../orka/data/shaders");

   FFT_CS : Fourier_Transforms.FFT := Fourier_Transforms.Create_FFT (Location_Shaders);

   FFT_W : Positive;
   FFT_H : Positive;
begin
   declare
      Local_Size_X : constant Size := Size (BO_1.Length / Count) / 2;
   begin
      if In_Vertical_Direction then
         FFT_W := Count;
         FFT_H := Positive (Local_Size_X);
      else
         FFT_W := Positive (Local_Size_X);
         FFT_H := Count;
      end if;
   end;

   if In_Vertical_Direction then
      --  Upload the data to the buffer in a transposed way in order to
      --  test the ability to apply the FFT in the vertical direction
      for I in 0 .. 7 loop
         for J in 0 .. Count - 1 loop
            BO_1.Set_Data (Numbers_Small (Size (I * 2 + 1) .. Size (I * 2 + 2)),
              Offset => (Count * I + J) * 2);
         end loop;
      end loop;
   else
      for Index in 0 .. Count - 1 loop
         BO_1.Set_Data (Numbers_Small, Offset => Numbers_Small'Length * Index);
      end loop;
   end if;

   ----------------------------------------------------------------------

   Ada.Text_IO.Put_Line ("Input:");
   Print_Array (Numbers_Small);

   Ada.Text_IO.Put_Line ("Output:");
   Print_Array (Output_Numbers_Small);

   Ada.Text_IO.Put_Line ("Vertical direction: " & In_Vertical_Direction'Image);
   Ada.Text_IO.Put_Line ("Rows:" & Count'Image);

   Ada.Text_IO.Put_Line ("Width: " & FFT_W'Image);
   Ada.Text_IO.Put_Line ("Height:" & FFT_H'Image);

   GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));

   Timer_1.Start;
   FFT_CS.Compute_FFT (BO_1, FFT_W, FFT_H, In_Vertical_Direction, Inverse => False);
   Timer_1.Stop;

   GL.Barriers.Memory_Barrier ((Shader_Storage | Buffer_Update => True, others => False));

   Print_Numbers ("FFT");

   Timer_1.Start;
   FFT_CS.Compute_FFT (BO_1, FFT_W, FFT_H, In_Vertical_Direction, Inverse => True);
   Timer_1.Stop;

   GL.Barriers.Memory_Barrier ((Buffer_Update => True, others => False));

   Print_Numbers ("Inverse FFT");
end Orka_6_FFT;
