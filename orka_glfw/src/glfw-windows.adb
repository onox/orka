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

with Ada.Task_Identification;
with Ada.Unchecked_Conversion;

with Glfw.API;
with Glfw.Enums;

package body Glfw.Windows is

   use Ada.Task_Identification;

   package Conversions is new System.Address_To_Access_Conversions (Window'Class);

   procedure Raw_Position_Callback (Raw  : System.Address;
                                    X, Y : Interfaces.C.int);
   procedure Raw_Size_Callback (Raw  : System.Address;
                                Width, Height : Interfaces.C.int);
   procedure Raw_Close_Callback (Raw  : System.Address);
   procedure Raw_Refresh_Callback (Raw  : System.Address);
   procedure Raw_Focus_Callback (Raw : System.Address; Focused : Bool);
   procedure Raw_Maximize_Callback (Raw : System.Address; Maximized : Bool);
   procedure Raw_Content_Scale_Callback (Raw : System.Address; X, Y : Interfaces.C.C_float);
   procedure Raw_Iconify_Callback (Raw : System.Address; Iconified : Bool);
   procedure Raw_Framebuffer_Size_Callback (Raw : System.Address;
                                            Width, Height : Interfaces.C.int);
   procedure Raw_Mouse_Button_Callback (Raw    : System.Address;
                                        Button : Input.Mouse.Button;
                                        State  : Input.Button_State;
                                        Mods   : Input.Keys.Modifiers);
   procedure Raw_Mouse_Position_Callback (Raw : System.Address;
                                          X, Y : Input.Mouse.Coordinate);
   procedure Raw_Mouse_Scroll_Callback (Raw  : System.Address;
                                        X, Y : Input.Mouse.Scroll_Offset);
   procedure Raw_Mouse_Enter_Callback (Raw  : System.Address;
                                       Action : Input.Mouse.Enter_Action);
   procedure Raw_Key_Callback (Raw : System.Address;
                               Key : Input.Keys.Key;
                               Scancode : Input.Keys.Scancode;
                               Action   : Input.Keys.Action;
                               Mods     : Input.Keys.Modifiers);
   procedure Raw_Character_Callback (Raw  : System.Address;
                                     Char : Interfaces.C.unsigned);

   procedure Raw_File_Drop_Callback
     (Raw   : System.Address;
      Count : Interfaces.C.int;
      Paths : Interfaces.C.Strings.chars_ptr_array);

   pragma Convention (C, Raw_Position_Callback);
   pragma Convention (C, Raw_Size_Callback);
   pragma Convention (C, Raw_Close_Callback);
   pragma Convention (C, Raw_Refresh_Callback);
   pragma Convention (C, Raw_Focus_Callback);
   pragma Convention (C, Raw_Maximize_Callback);
   pragma Convention (C, Raw_Content_Scale_Callback);
   pragma Convention (C, Raw_Iconify_Callback);
   pragma Convention (C, Raw_Framebuffer_Size_Callback);
   pragma Convention (C, Raw_Mouse_Button_Callback);
   pragma Convention (C, Raw_Mouse_Position_Callback);
   pragma Convention (C, Raw_Mouse_Scroll_Callback);
   pragma Convention (C, Raw_Mouse_Enter_Callback);
   pragma Convention (C, Raw_Key_Callback);
   pragma Convention (C, Raw_Character_Callback);
   pragma Convention (C, Raw_File_Drop_Callback);

   function Window_Ptr (Raw : System.Address) return not null access Window'Class is
   begin
      return Conversions.To_Pointer (API.Get_Window_User_Pointer (Raw));
   end Window_Ptr;

   procedure Raw_Position_Callback (Raw  : System.Address;
                                    X, Y : Interfaces.C.int) is
   begin
      Window_Ptr (Raw).Position_Changed (Integer (X), Integer (Y));
   end Raw_Position_Callback;

   procedure Raw_Size_Callback (Raw  : System.Address;
                                Width, Height : Interfaces.C.int) is
   begin
      Window_Ptr (Raw).Size_Changed (Natural (Width), Natural (Height));
   end Raw_Size_Callback;

   procedure Raw_Close_Callback (Raw  : System.Address) is
   begin
      Window_Ptr (Raw).Close_Requested;
   end Raw_Close_Callback;

   procedure Raw_Refresh_Callback (Raw  : System.Address) is
   begin
      Window_Ptr (Raw).Refresh;
   end Raw_Refresh_Callback;

   procedure Raw_Focus_Callback (Raw : System.Address; Focused : Bool) is
   begin
      Window_Ptr (Raw).Focus_Changed (Boolean (Focused));
   end Raw_Focus_Callback;

   procedure Raw_Maximize_Callback (Raw : System.Address; Maximized : Bool) is
   begin
      Window_Ptr (Raw).Maximize_Changed (Boolean (Maximized));
   end Raw_Maximize_Callback;

   procedure Raw_Content_Scale_Callback
     (Raw : System.Address; X, Y : Interfaces.C.C_float) is
   begin
      Window_Ptr (Raw).Content_Scale_Changed (Float (X), Float (Y));
   end Raw_Content_Scale_Callback;

   procedure Raw_Iconify_Callback (Raw : System.Address; Iconified : Bool) is
   begin
      Window_Ptr (Raw).Iconification_Changed (Boolean (Iconified));
   end Raw_Iconify_Callback;

   procedure Raw_Framebuffer_Size_Callback (Raw : System.Address;
                                            Width, Height : Interfaces.C.int) is
   begin
      Window_Ptr (Raw).Framebuffer_Size_Changed (Natural (Width), Natural (Height));
   end Raw_Framebuffer_Size_Callback;

   procedure Raw_Mouse_Button_Callback (Raw    : System.Address;
                                        Button : Input.Mouse.Button;
                                        State  : Input.Button_State;
                                        Mods   : Input.Keys.Modifiers) is
   begin
      Window_Ptr (Raw).Mouse_Button_Changed (Button, State, Mods);
   end Raw_Mouse_Button_Callback;

   procedure Raw_Mouse_Position_Callback (Raw : System.Address;
                                          X, Y : Input.Mouse.Coordinate) is
   begin
      Window_Ptr (Raw).Mouse_Position_Changed (X, Y);
   end Raw_Mouse_Position_Callback;

   procedure Raw_Mouse_Scroll_Callback (Raw  : System.Address;
                                        X, Y : Input.Mouse.Scroll_Offset) is
   begin
      Window_Ptr (Raw).Mouse_Scrolled (X, Y);
   end Raw_Mouse_Scroll_Callback;

   procedure Raw_Mouse_Enter_Callback (Raw  : System.Address;
                                       Action : Input.Mouse.Enter_Action) is
   begin
      Window_Ptr (Raw).Mouse_Entered (Action);
   end Raw_Mouse_Enter_Callback;

   procedure Raw_Key_Callback (Raw : System.Address;
                               Key : Input.Keys.Key;
                               Scancode : Input.Keys.Scancode;
                               Action   : Input.Keys.Action;
                               Mods     : Input.Keys.Modifiers) is
   begin
      Window_Ptr (Raw).Key_Changed (Key, Scancode, Action, Mods);
   end Raw_Key_Callback;

   procedure Raw_Character_Callback (Raw  : System.Address;
                                     Char : Interfaces.C.unsigned) is
      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned, Wide_Wide_Character);
   begin
      Window_Ptr (Raw).Character_Entered (Convert (Char));
   end Raw_Character_Callback;

   procedure Raw_File_Drop_Callback
     (Raw   : System.Address;
      Count : Interfaces.C.int;
      Paths : Interfaces.C.Strings.chars_ptr_array)
   is
      List : Path_List (1 .. Positive (Count));
   begin
      for Index in List'Range loop
         List (Index) := SU.To_Unbounded_String
           (Interfaces.C.Strings.Value (Paths (Interfaces.C.size_t (Index))));
      end loop;
      Window_Ptr (Raw).Files_Dropped (List);
   end Raw_File_Drop_Callback;

   procedure Init (Object        : not null access Window;
                   Width, Height : Size;
                   Title         : String;
                   Monitor       : Monitors.Monitor := Monitors.No_Monitor;
                   Share_Resources_With : access Window'Class := null)
   is
      pragma Assert (Current_Task = Environment_Task);

      use type System.Address;

      C_Title : constant Interfaces.C.char_array := Interfaces.C.To_C (Title);
      Share : System.Address;
   begin
      if Object.Handle /= System.Null_Address then
         raise Operation_Exception with "Window has already been initialized";
      end if;
      if Share_Resources_With = null then
         Share := System.Null_Address;
      else
         Share := Share_Resources_With.Handle;
      end if;

      Object.Handle := API.Create_Window (Interfaces.C.int (Width),
                                          Interfaces.C.int (Height),
                                          C_Title, Monitor.Raw_Pointer, Share);
      if Object.Handle = System.Null_Address then
         raise Creation_Error;
      end if;
      API.Set_Window_User_Pointer (Object.Handle, Conversions.To_Address
        (Conversions.Object_Pointer'(Object.all'Unchecked_Access)));
   end Init;

   function Initialized (Object : not null access Window) return Boolean is
      use type System.Address;
   begin
      return Object.Handle /= System.Null_Address;
   end Initialized;

   procedure Destroy (Object : not null access Window) is
      pragma Assert (Current_Task = Environment_Task);
   begin
      API.Destroy_Window (Object.Handle);
      Object.Handle := System.Null_Address;
   end Destroy;

   procedure Show (Object : not null access Window) is
   begin
      API.Show_Window (Object.Handle);
   end Show;

   procedure Hide (Object : not null access Window) is
   begin
      API.Hide_Window (Object.Handle);
   end Hide;

   procedure Focus (Object : not null access Window) is
   begin
      API.Focus_Window (Object.Handle);
   end Focus;

   procedure Maximize (Object : not null access Window) is
   begin
      API.Maximize_Window (Object.Handle);
   end Maximize;

   procedure Request_Attention (Object : not null access Window) is
   begin
      API.Request_Window_Attention (Object.Handle);
   end Request_Attention;

   function Get_Monitor (Object : not null access Window) return Monitors.Monitor is
   begin
      return Monitors.To_Monitor (API.Get_Window_Monitor (Object.Handle));
   exception
      when Operation_Exception =>
         return Monitors.No_Monitor;
   end Get_Monitor;

   procedure Set_Monitor
     (Object  : not null access Window;
      Monitor : Monitors.Monitor := Monitors.No_Monitor)
   is
      use type Monitors.Monitor;

      OS : Windowed_Size renames Object.Original_Size;
   begin
      if Monitor = Monitors.No_Monitor then
         --  Restore saved position and size
         API.Set_Window_Monitor (Object.Handle, Monitor.Raw_Pointer,
           OS.X, OS.Y, OS.Width, OS.Height, 0);
      else
         declare
            Mode : constant Monitors.Video_Mode := Monitor.Current_Video_Mode;
         begin
            --  Save position and size
            Object.Get_Position (OS.X, OS.Y);
            Object.Get_Size (OS.Width, OS.Height);

            API.Set_Window_Monitor (Object.Handle, Monitor.Raw_Pointer,
              0, 0, Mode.Width, Mode.Height, Mode.Refresh_Rate);
         end;
      end if;
   end Set_Monitor;

   procedure Set_Title (Object : not null access Window; Value : String) is
      pragma Assert (Current_Task = Environment_Task);
   begin
      API.Set_Window_Title (Object.Handle, Interfaces.C.To_C (Value));
   end Set_Title;

   function Key_State (Object : not null access Window; Key : Input.Keys.Key)
                       return Input.Button_State is
   begin
      return API.Get_Key (Object.Handle, Key);
   end Key_State;

   function Mouse_Button_State (Object : not null access Window;
                                Button : Input.Mouse.Button)
                                return Input.Button_State is
   begin
      return API.Get_Mouse_Button (Object.Handle, Button);
   end Mouse_Button_State;

   function Get_Input_Toggle (Object : not null access Window;
                              Kind   : Input.Input_Toggle) return Boolean is
   begin
      return Boolean (API.Get_Input_Mode (Object.Handle, Kind));
   end Get_Input_Toggle;

   procedure Set_Input_Toggle (Object : not null access Window;
                               Kind   : Input.Input_Toggle;
                               Value  : Boolean) is
   begin
      API.Set_Input_Mode (Object.Handle, Kind, Bool (Value));
   end Set_Input_Toggle;

   function Get_Cursor_Mode (Object : not null access Window)
     return Input.Mouse.Cursor_Mode is
   begin
      return API.Get_Input_Mode (Object.Handle, Enums.Mouse_Cursor);
   end Get_Cursor_Mode;

   procedure Set_Cursor_Mode (Object : not null access Window;
                              Mode   : Input.Mouse.Cursor_Mode) is
   begin
      API.Set_Input_Mode (Object.Handle, Enums.Mouse_Cursor, Mode);
   end Set_Cursor_Mode;

   procedure Get_Cursor_Pos (Object : not null access Window;
                             X, Y   : out Input.Mouse.Coordinate) is
   begin
      API.Get_Cursor_Pos (Object.Handle, X, Y);
   end Get_Cursor_Pos;

   procedure Set_Cursor_Pos (Object : not null access Window;
                             X, Y   : Input.Mouse.Coordinate) is
   begin
      API.Set_Cursor_Pos (Object.Handle, X, Y);
   end Set_Cursor_Pos;

   procedure Get_Position (Object : not null access Window;
                           X, Y : out Coordinate) is
   begin
      API.Get_Window_Pos (Object.Handle, X, Y);
   end Get_Position;

   procedure Set_Position (Object : not null access Window;
                           X, Y : Coordinate) is
   begin
      API.Set_Window_Pos (Object.Handle, X, Y);
   end Set_Position;

   procedure Set_Aspect_Ratio
     (Object : not null access Window;
      Numer, Denom : Size) is
   begin
      API.Set_Window_Aspect_Ratio (Object.Handle, Numer, Denom);
   end Set_Aspect_Ratio;

   procedure Set_Window_Size_Limits
     (Object : not null access Window;
      Min_Width, Min_Height, Max_Width, Max_Height : Size) is
   begin
      API.Set_Window_Size_Limits
        (Object.Handle, Min_Width, Min_Height, Max_Width, Max_Height);
   end Set_Window_Size_Limits;

   procedure Get_Size (Object : not null access Window;
                       Width, Height : out Size) is
   begin
      API.Get_Window_Size (Object.Handle, Width, Height);
   end Get_Size;

   procedure Set_Size (Object : not null access Window;
                       Width, Height : Size) is
   begin
      API.Set_Window_Size (Object.Handle, Width, Height);
   end Set_Size;

   procedure Get_Content_Scale
     (Object : not null access Window; X, Y : out Float)
   is
      X_Raw, Y_Raw : Interfaces.C.C_float;
   begin
      API.Get_Window_Content_Scale (Object.Handle, X_Raw, Y_Raw);
      X := Float (X_Raw);
      Y := Float (Y_Raw);
   end Get_Content_Scale;

   function Get_Opacity (Object : not null access Window) return Opacity is
     (API.Get_Window_Opacity (Object.Handle));

   procedure Set_Opacity (Object : not null access Window; Value : Opacity) is
   begin
      API.Set_Window_Opacity (Object.Handle, Value);
   end Set_Opacity;

   procedure Get_Frame_Size
     (Object : not null access Window;
      Left, Top, Right, Bottom : out Size) is
   begin
      API.Get_Window_Frame_Size (Object.Handle, Left, Top, Right, Bottom);
   end Get_Frame_Size;

   procedure Get_Framebuffer_Size (Object : not null access Window;
                                   Width, Height : out Size) is
   begin
      API.Get_Framebuffer_Size (Object.Handle, Width, Height);
   end Get_Framebuffer_Size;

   function Focused (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Focused))));

   function Iconified (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Iconified))));

   function Resizable (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Resizable))));

   function Visible (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Visible))));

   function Decorated (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Decorated))));

   function Auto_Iconified (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Auto_Iconify))));

   function Floating (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Floating))));

   function Maximized (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Maximized))));

   function Transparent_Framebuffer (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Transparent))));

   function Hovered (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Hovered))));

   function Focused_On_Show (Object : not null access Window) return Boolean is
     (Boolean (Bool'(API.Get_Window_Attrib (Object.Handle, Enums.Focus_On_Show))));

   procedure Set_Resizable (Object : not null access Window; Enable : Boolean) is
   begin
      API.Set_Window_Attrib (Object.Handle, Enums.Resizable, Bool (Enable));
   end Set_Resizable;

   procedure Set_Decorated (Object : not null access Window; Enable : Boolean) is
   begin
      API.Set_Window_Attrib (Object.Handle, Enums.Decorated, Bool (Enable));
   end Set_Decorated;

   procedure Set_Auto_Iconify (Object : not null access Window; Enable : Boolean) is
   begin
      API.Set_Window_Attrib (Object.Handle, Enums.Auto_Iconify, Bool (Enable));
   end Set_Auto_Iconify;

   procedure Set_Floating (Object : not null access Window; Enable : Boolean) is
   begin
      API.Set_Window_Attrib (Object.Handle, Enums.Floating, Bool (Enable));
   end Set_Floating;

   procedure Set_Focus_On_Show (Object : not null access Window; Enable : Boolean) is
   begin
      API.Set_Window_Attrib (Object.Handle, Enums.Focus_On_Show, Bool (Enable));
   end Set_Focus_On_Show;

   function Should_Close (Object : not null access constant Window) return Boolean is
   begin
      return Boolean (API.Window_Should_Close (Object.Handle));
   end Should_Close;

   procedure Set_Should_Close (Object : not null access Window;
                               Value : Boolean) is
   begin
      API.Set_Window_Should_Close (Object.Handle, Bool (Value));
   end Set_Should_Close;

   procedure Enable_Callback (Object  : not null access Window;
                              Subject : Callbacks.Kind) is
   begin
      case Subject is
         when Callbacks.Position =>
            API.Set_Window_Pos_Callback (Object.Handle, Raw_Position_Callback'Access);
         when Callbacks.Size =>
            API.Set_Window_Size_Callback (Object.Handle, Raw_Size_Callback'Access);
         when Callbacks.Close =>
            API.Set_Window_Close_Callback (Object.Handle, Raw_Close_Callback'Access);
         when Callbacks.Refresh =>
            API.Set_Window_Refresh_Callback (Object.Handle, Raw_Refresh_Callback'Access);
         when Callbacks.Focus =>
            API.Set_Window_Focus_Callback (Object.Handle, Raw_Focus_Callback'Access);
         when Callbacks.Maximize =>
            API.Set_Window_Maximize_Callback (Object.Handle, Raw_Maximize_Callback'Access);
         when Callbacks.Content_Scale =>
            API.Set_Window_Content_Scale_Callback
              (Object.Handle, Raw_Content_Scale_Callback'Access);
         when Callbacks.Iconify =>
            API.Set_Window_Iconify_Callback (Object.Handle, Raw_Iconify_Callback'Access);
         when Callbacks.Framebuffer_Size =>
            API.Set_Framebuffer_Size_Callback
              (Object.Handle, Raw_Framebuffer_Size_Callback'Access);
         when Callbacks.Mouse_Button =>
            API.Set_Mouse_Button_Callback (Object.Handle, Raw_Mouse_Button_Callback'Access);
         when Callbacks.Mouse_Position =>
            API.Set_Cursor_Pos_Callback (Object.Handle, Raw_Mouse_Position_Callback'Access);
         when Callbacks.Mouse_Scroll =>
            API.Set_Scroll_Callback (Object.Handle, Raw_Mouse_Scroll_Callback'Access);
         when Callbacks.Mouse_Enter =>
            API.Set_Cursor_Enter_Callback (Object.Handle, Raw_Mouse_Enter_Callback'Access);
         when Callbacks.Key =>
            API.Set_Key_Callback (Object.Handle, Raw_Key_Callback'Access);
         when Callbacks.Char =>
            API.Set_Char_Callback (Object.Handle, Raw_Character_Callback'Access);
         when Callbacks.File_Drop =>
            API.Set_Drop_Callback (Object.Handle, Raw_File_Drop_Callback'Access);
      end case;
   end Enable_Callback;

   procedure Disable_Callback (Object  : not null access Window;
                               Subject : Callbacks.Kind) is
   begin
      case Subject is
         when Callbacks.Position =>
            API.Set_Window_Pos_Callback (Object.Handle, null);
         when Callbacks.Size =>
            API.Set_Window_Size_Callback (Object.Handle, null);
         when Callbacks.Close =>
            API.Set_Window_Close_Callback (Object.Handle, null);
         when Callbacks.Refresh =>
            API.Set_Window_Refresh_Callback (Object.Handle, null);
         when Callbacks.Focus =>
            API.Set_Window_Focus_Callback (Object.Handle, null);
         when Callbacks.Maximize =>
            API.Set_Window_Maximize_Callback (Object.Handle, null);
         when Callbacks.Content_Scale =>
            API.Set_Window_Content_Scale_Callback (Object.Handle, null);
         when Callbacks.Iconify =>
            API.Set_Window_Iconify_Callback (Object.Handle, null);
         when Callbacks.Framebuffer_Size =>
            API.Set_Framebuffer_Size_Callback (Object.Handle, null);
         when Callbacks.Mouse_Button =>
            API.Set_Mouse_Button_Callback (Object.Handle, null);
         when Callbacks.Mouse_Position =>
            API.Set_Cursor_Pos_Callback (Object.Handle, null);
         when Callbacks.Mouse_Scroll =>
            API.Set_Scroll_Callback (Object.Handle, null);
         when Callbacks.Mouse_Enter =>
            API.Set_Cursor_Enter_Callback (Object.Handle, null);
         when Callbacks.Key =>
            API.Set_Key_Callback (Object.Handle, null);
         when Callbacks.Char =>
            API.Set_Char_Callback (Object.Handle, null);
         when Callbacks.File_Drop =>
            API.Set_Drop_Callback (Object.Handle, null);
      end case;
   end Disable_Callback;

   procedure Get_OpenGL_Version (Object : not null access Window;
                                 Major, Minor, Revision : out Natural) is
      Value : Interfaces.C.int;
   begin
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Major);
      Major := Natural (Value);
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Version_Minor);
      Minor := Natural (Value);
      Value := API.Get_Window_Attrib (Object.Handle, Enums.Context_Revision);
      Revision := Natural (Value);
   end Get_OpenGL_Version;

end Glfw.Windows;
