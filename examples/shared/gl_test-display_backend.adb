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

with Ada.Text_IO;

with Glfw.Windows.Context;
with Glfw.Windows.Hints;
with Glfw.Errors;

package body GL_Test.Display_Backend is

   generic
      type Source is digits <>;
      type Target is digits <>;
   function Clamp (Value : in Source) return Target;

   function Clamp (Value : in Source) return Target is
      A : constant Source := Source'Min (Source (Target'Last), Value);
      B : constant Source := Source'Max (Source (Target'First), A);
   begin
      return Target (B);
   end Clamp;

   generic
      type Source is digits <>;
      type Target is digits <>;
   function Normalize_Periodic (Value : in Source) return Target;

   function Normalize_Periodic (Value : in Source) return Target is
      Target_Min   : constant Source := Source (Target'First);
      Target_Range : constant Source := Source (Target'Last - Target'First);
   begin
      return Target (Value - Target_Range * Source'Floor ((Value - Target_Min) / Target_Range));
   end Normalize_Periodic;

   function Clamp_Zoom is new Clamp (Float, Mouse_Zoom);
   function Normalize_Mouse is new Normalize_Periodic (Float, Mouse_Pos);

   overriding
   procedure Close_Requested (Object : not null access Test_Window) is
   begin
      Object.Destroy;
   end Close_Requested;

   overriding
   procedure Key_Changed (Object   : not null access Test_Window;
                          Key      : Glfw.Input.Keys.Key;
                          Scancode : Glfw.Input.Keys.Scancode;
                          Action   : Glfw.Input.Keys.Action;
                          Mods     : Glfw.Input.Keys.Modifiers) is
      use Glfw.Input.Keys;
   begin
      if Key = Escape and Action = Press then
         Object.Set_Should_Close (True);
      elsif Key = Space and Action = Press then
         Object.Effect := Object.Effect + 1;
      end if;
      if Key = W and Action = Press then
         Object.Pos_X := Object.Pos_X + 0.1;
      end if;
      if Key = S and Action = Press then
         Object.Pos_X := Object.Pos_X - 0.1;
      end if;
      if Key = A and Action = Press then
         Object.Pos_Y := Object.Pos_Y + 0.1;
      end if;
      if Key = D and Action = Press then
         Object.Pos_Y := Object.Pos_Y - 0.1;
      end if;
      if Key = Q and Action = Press then
         Object.Pos_Z := Object.Pos_Z + 0.1;
      end if;
      if Key = E and Action = Press then
         Object.Pos_Z := Object.Pos_Z - 0.1;
      end if;
   end Key_Changed;

   overriding
   procedure Mouse_Position_Changed (Object : not null access Test_Window;
                                     X, Y   : Glfw.Input.Mouse.Coordinate) is
   begin
      if Object.Moving_Camera then
         Object.Offset_X := Normalize_Mouse ((Float (X) - Object.Mouse_X) / 2.0 + Object.Prev_X);
         Object.Offset_Y := Normalize_Mouse ((Float (Y) - Object.Mouse_Y) / 1.0 + Object.Prev_Y);
      else
         Object.Mouse_X := Float (X);
         Object.Mouse_Y := Float (Y);
      end if;
   end Mouse_Position_Changed;

   overriding
   procedure Mouse_Scrolled (Object : not null access Test_Window;
                             X, Y   : Glfw.Input.Mouse.Scroll_Offset) is
   begin
      Object.Zoom_Distance := Clamp_Zoom (Object.Zoom_Distance - Float (Y) / 2.0);
   end Mouse_Scrolled;

   overriding
   procedure Mouse_Button_Changed (Object  : not null access Test_Window;
                                   Button  : Glfw.Input.Mouse.Button;
                                   State   : Glfw.Input.Button_State;
                                   Mods    : Glfw.Input.Keys.Modifiers) is
      use Glfw.Input.Mouse;
      use Glfw.Input;
   begin
      if Button = Right_Button then
         Object.Moving_Camera := State = Pressed;
         if Object.Moving_Camera then
            Object.Set_Cursor_Mode (Disabled);
         else
            Object.Set_Cursor_Mode (Normal);
            Object.Prev_X := Object.Offset_X;
            Object.Prev_Y := Object.Offset_Y;
         end if;
      end if;
   end Mouse_Button_Changed;

   Main_Window : constant Test_Window_Access := new Test_Window;

   procedure Print_Error (Code : Glfw.Errors.Kind; Description : String) is
   begin
      Ada.Text_IO.Put_Line ("Error occured (" & Glfw.Errors.Kind'Image (Code) & "): " & Description);
   end Print_Error;

   procedure Enable_Print_Errors is
   begin
      Glfw.Errors.Set_Callback (Print_Error'Access);
   end Enable_Print_Errors;

   function Init
     (Major, Minor : Natural;
      Width, Height : Natural;
      Visible, Resizable : Boolean := True;
      Depth_Bits : Natural := 0;
      Debug : Boolean := False;
      Samples : Natural := 0) return Boolean is
   begin
      --  Initialize Glfw
      Enable_Print_Errors;
      Glfw.Init;

      --  Initialize OpenGL context
      Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Major, Minor);
      Glfw.Windows.Hints.Set_Forward_Compat (True);
      Glfw.Windows.Hints.Set_Profile (Glfw.Windows.Context.Core_Profile);
      Glfw.Windows.Hints.Set_Debug_Context (Debug);

      if not Main_Window.Initialized then
         Glfw.Windows.Hints.Set_Visible (Visible);
         Glfw.Windows.Hints.Set_Resizable (Resizable);
         Glfw.Windows.Hints.Set_Depth_Bits (Depth_Bits);
         if Samples > 0 then
            Glfw.Windows.Hints.Set_Samples (Samples);
         end if;

         Main_Window.Init (Glfw.Size (Width), Glfw.Size (Height), "");
      end if;
      if Visible then
         Main_Window.Show;
      end if;

      -- Callbacks
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Close);
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Key);

      Glfw.Windows.Context.Make_Current (Main_Window);
      return Main_Window.Initialized;
   end Init;

   function Get_Window return Glfw.Windows.Window_Reference is
   begin
      return Glfw.Windows.Window (Main_Window.all)'Access;
   end Get_Window;

   procedure Swap_Buffers_And_Poll_Events is
   begin
      Glfw.Windows.Context.Swap_Buffers (Main_Window);
      Glfw.Input.Poll_Events;
   end Swap_Buffers_And_Poll_Events;

   procedure Set_Window_Title (Value : String) is
   begin
      Main_Window.Set_Title (Value);
   end Set_Window_Title;

   procedure Close_Window is
   begin
      Main_Window.Destroy;
   end Close_Window;

   procedure Shutdown renames Glfw.Shutdown;

   function Get_Mouse_X return Float is
   begin
      return Main_Window.Offset_X;
   end Get_Mouse_X;

   function Get_Mouse_Y return Float is
   begin
      return Main_Window.Offset_Y;
   end Get_Mouse_Y;

   function Get_Zoom_Distance return Float is
   begin
      return Float (Main_Window.Zoom_Distance);
   end Get_Zoom_Distance;

   procedure Set_Zoom_Distance (Distance : Float) is
   begin
      Main_Window.Zoom_Distance := Distance;
   end Set_Zoom_Distance;

   function Get_Effect (Maximum : Positive) return Integer is
   begin
      return Integer (Main_Window.Effect) mod Maximum;
   end Get_Effect;

   function Get_Pos_X return Float is
     (Main_Window.Pos_X);

   function Get_Pos_Y return Float is
     (Main_Window.Pos_Y);

   function Get_Pos_Z return Float is
     (Main_Window.Pos_Z);

end GL_Test.Display_Backend;
