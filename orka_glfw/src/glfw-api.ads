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
with Interfaces.C.Pointers;
with System;

with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Input.Joysticks;
with Glfw.Monitors;
with Glfw.Errors;
with Glfw.Enums;
with Glfw.Windows.Context;

private package Glfw.API is
   pragma Preelaborate;

   -----------------------------------------------------------------------------
   --  Types
   -----------------------------------------------------------------------------

   type Address_List is array (Positive range <>) of aliased System.Address;
   pragma Convention (C, Address_List);

   package Address_List_Pointers is new Interfaces.C.Pointers
     (Positive, System.Address, Address_List, 0);  --  0 = System.Null_Address

   package VMode_List_Pointers is new Interfaces.C.Pointers
     (Positive, Monitors.Video_Mode, Monitors.Video_Mode_List, (others => 0));

   package Axis_Position_List_Pointers is new Interfaces.C.Pointers
     (Positive, Input.Joysticks.Axis_Position, Input.Joysticks.Axis_Positions,
      0.0);

   package Joystick_Button_State_List_Pointers is new Interfaces.C.Pointers
     (Positive, Input.Joysticks.Joystick_Button_State,
      Input.Joysticks.Joystick_Button_States, Input.Joysticks.Released);

   package Joystick_Hat_State_List_Pointers is new Interfaces.C.Pointers
     (Positive, Input.Joysticks.Joystick_Hat_State,
      Input.Joysticks.Joystick_Hat_States, Input.Joysticks.Centered);

   package Unsigned_Short_List_Pointers is new Interfaces.C.Pointers
     (Positive, Interfaces.C.unsigned_short, Glfw.Monitors.Gamma_Value_Array, 0);

   type Raw_Gamma_Ramp is record
      Red, Green, Blue : Unsigned_Short_List_Pointers.Pointer;
      Size : Interfaces.C.unsigned;
   end record;
   pragma Convention (C, Raw_Gamma_Ramp);

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   type Error_Callback is access procedure (Code : Errors.Kind;
                                            Description : C.Strings.chars_ptr);
   type Window_Position_Callback is access procedure
     (Window : System.Address; X, Y : C.int);
   type Window_Size_Callback is access procedure
     (Window : System.Address; Width, Height : C.int);
   type Window_Close_Callback is access procedure
     (Window : System.Address);
   type Window_Refresh_Callback is access procedure
     (Window : System.Address);
   type Window_Focus_Callback is access procedure
     (Window : System.Address; Focussed : Bool);
   type Window_Maximize_Callback is access procedure
     (Window : System.Address; Maximized : Bool);
   type Window_Iconify_Callback is access procedure
     (Window : System.Address; Iconified : Bool);
   type Window_Content_Scale_Callback is access procedure
     (Window : System.Address;
      X, Y   : C.C_float);
   type Framebuffer_Size_Callback is access procedure
     (Window : System.Address; Width, Height : C.int);
   type Mouse_Button_Callback is access procedure (Window : System.Address;
                                                   Button : Input.Mouse.Button;
                                                   State  : Input.Button_State;
                                                   Mods   : Input.Keys.Modifiers);
   type Cursor_Position_Callback is access procedure
     (Window : System.Address; X, Y : Input.Mouse.Coordinate);
   type Cursor_Enter_Callback is access procedure
     (Window : System.Address; Action : Input.Mouse.Enter_Action);
   type Scroll_Callback is access procedure
     (Window : System.Address; X, Y : Input.Mouse.Scroll_Offset);
   type Key_Callback is access procedure (Window   : System.Address;
                                          Key      : Input.Keys.Key;
                                          Scancode : Input.Keys.Scancode;
                                          Action   : Input.Keys.Action;
                                          Mods     : Input.Keys.Modifiers);
   type Character_Callback is access procedure
     (Window : System.Address; Unicode_Char : Interfaces.C.unsigned);
   type Monitor_Callback is access procedure
     (Monitor : System.Address; Event : Monitors.Event);
   type File_Drop_Callback is access procedure
     (Window : System.Address; Count : C.int; Paths : C.Strings.chars_ptr_array);
   type Joystick_Callback is access procedure
     (Joystick : Enums.Joystick_ID; Event : Input.Joysticks.Connect_State);

   pragma Convention (C, Error_Callback);
   pragma Convention (C, Window_Position_Callback);
   pragma Convention (C, Window_Size_Callback);
   pragma Convention (C, Window_Close_Callback);
   pragma Convention (C, Window_Refresh_Callback);
   pragma Convention (C, Window_Focus_Callback);
   pragma Convention (C, Window_Maximize_Callback);
   pragma Convention (C, Window_Iconify_Callback);
   pragma Convention (C, Window_Content_Scale_Callback);
   pragma Convention (C, Framebuffer_Size_Callback);
   pragma Convention (C, Mouse_Button_Callback);
   pragma Convention (C, Cursor_Position_Callback);
   pragma Convention (C, Cursor_Enter_Callback);
   pragma Convention (C, Scroll_Callback);
   pragma Convention (C, Key_Callback);
   pragma Convention (C, Character_Callback);
   pragma Convention (C, Monitor_Callback);
   pragma Convention (C, File_Drop_Callback);
   pragma Convention (C, Joystick_Callback);

   -----------------------------------------------------------------------------
   --  Basics
   -----------------------------------------------------------------------------

   function Init return C.int;
   pragma Import (Convention => C, Entity => Init,
                  External_Name => "glfwInit");

   procedure Init_Hint (Hint : Enums.Init_Hint; Value : Bool)
     with Import, Convention => C, External_Name => "glfwInitHint";

   procedure Glfw_Terminate;
   pragma Import (Convention => C, Entity => Glfw_Terminate,
                  External_Name => "glfwTerminate");

   procedure Get_Version (Major, Minor, Revision : out C.int);
   pragma Import (Convention => C, Entity => Get_Version,
                  External_Name => "glfwGetVersion");

   function Get_Version_String return C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Version_String,
                  External_Name => "glfwGetVersionString");

   function Extension_Supported (Name : C.char_array) return Bool;
   pragma Import (Convention => C, Entity => Extension_Supported,
                  External_Name => "glfwExtensionSupported");

   function Set_Error_Callback (CB_Fun : Error_Callback) return Error_Callback;
   pragma Import (Convention => C, Entity => Set_Error_Callback,
                  External_Name => "glfwSetErrorCallback");

   -----------------------------------------------------------------------------
   --  Monitors
   -----------------------------------------------------------------------------

   function Get_Monitors (Count : access Interfaces.C.int)
                          return Address_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Monitors,
                  External_Name => "glfwGetMonitors");

   function Get_Primary_Monitor return System.Address;
   pragma Import (Convention => C, Entity => Get_Primary_Monitor,
                  External_Name => "glfwGetPrimaryMonitor");

   procedure Get_Monitor_Pos (Monitor : System.Address;
                              XPos, YPos : out Interfaces.C.int);
   pragma Import (Convention => C, Entity => Get_Monitor_Pos,
                  External_Name => "glfwGetMonitorPos");

   procedure Get_Monitor_Physical_Size (Monitor : System.Address;
                                        Width, Height : out Interfaces.C.int);
   pragma Import (Convention => C, Entity => Get_Monitor_Physical_Size,
                  External_Name => "glfwGetMonitorPhysicalSize");

   procedure Get_Monitor_Content_Scale
     (Monitor : System.Address;
      X, Y    : out Interfaces.C.C_float)
   with Import, Convention => C, External_Name => "glfwGetMonitorContentScale";

   procedure Get_Monitor_Workarea
     (Monitor : System.Address;
      X, Y, Width, Height : out Interfaces.C.int)
   with Import, Convention => C, External_Name => "glfwGetMonitorWorkarea";

   function Get_Monitor_Name (Monitor : System.Address)
                              return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Monitor_Name,
                  External_Name => "glfwGetMonitorName");

   procedure Set_Monitor_User_Pointer
     (Monitor : System.Address;
      Pointer : System.Address)
   with Import, Convention => C, External_Name => "glfwSetMonitorUserPointer";

   function Get_Monitor_User_Pointer
     (Monitor : System.Address) return System.Address
   with Import, Convention => C, External_Name => "glfwGetMonitorUserPointer";

   procedure Set_Monitor_Callback
     (Monitor : System.Address;
      CB_Fun  : Monitor_Callback)
   with Import, Convention => C, External_Name => "glfwSetMonitorCallback";

   function Get_Video_Modes (Monitor : System.Address;
                             Count : access Interfaces.C.int)
                             return VMode_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Video_Modes,
                  External_Name => "glfwGetVideoModes");

   function Get_Video_Mode (Monitor : System.Address)
                            return access constant Monitors.Video_Mode;
   pragma Import (Convention => C, Entity => Get_Video_Mode,
                  External_Name => "glfwGetVideoMode");

   procedure Set_Gamma (Monitor : System.Address; Gamma : Interfaces.C.C_float);
   pragma Import (Convention => C, Entity => Set_Gamma,
                  External_Name => "glfwSetGamma");

   function Get_Gamma_Ramp (Monitor : System.Address)
                            return access constant Raw_Gamma_Ramp;
   pragma Import (Convention => C, Entity => Get_Gamma_Ramp,
                  External_Name => "glfwGetGammaRamp");

   procedure Set_Gamma_Ramp (Monitor : System.Address;
                             Value   : access constant Raw_Gamma_Ramp);
   pragma Import (Convention => C, Entity => Set_Gamma_Ramp,
                  External_Name => "glfwSetGammaRamp");

   -----------------------------------------------------------------------------
   --  Windows
   -----------------------------------------------------------------------------

   procedure Default_Window_Hints;
   pragma Import (Convention => C, Entity => Default_Window_Hints,
                  External_Name => "glfwDefaultWindowHints");

   procedure Window_Hint (Target : Glfw.Enums.Window_Hint; Info : C.int);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint; Info : Bool);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Windows.Context.OpenGL_Profile_Kind);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Windows.Context.API_Kind);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Glfw.Windows.Context.Robustness_Kind);
   procedure Window_Hint (Target : Glfw.Enums.Window_Hint;
                          Info   : Windows.Context.Release_Behavior);
   pragma Import (Convention => C, Entity => Window_Hint,
                  External_Name => "glfwWindowHint");

   function Create_Window (Width, Height : Interfaces.C.int;
                           Title         : Interfaces.C.char_array;
                           Monitor       : System.Address;
                           Share         : System.Address)
                           return System.Address;
   pragma Import (Convention => C, Entity => Create_Window,
                  External_Name => "glfwCreateWindow");

   procedure Destroy_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Destroy_Window,
                  External_Name => "glfwDestroyWindow");

   procedure Focus_Window (Window : System.Address)
     with Import, Convention => C, External_Name => "glfwFocusWindow";

   procedure Maximize_Window (Window : System.Address)
     with Import, Convention => C, External_Name => "glfwMaximizeWindow";

   function Window_Should_Close (Window : System.Address)
                                 return Bool;
   pragma Import (Convention => C, Entity => Window_Should_Close,
                  External_Name => "glfwWindowShouldClose");

   procedure Request_Window_Attention (Window : System.Address)
     with Import, Convention => C, External_Name => "glfwRequestWindowAttention";

   procedure Set_Window_Should_Close (Window : System.Address;
                                      Value  : Bool);
   pragma Import (Convention => C, Entity => Set_Window_Should_Close,
                  External_Name => "glfwSetWindowShouldClose");

   procedure Set_Window_Title (Window : System.Address;
                               Title  : Interfaces.C.char_array);
   pragma Import (Convention => C, Entity => Set_Window_Title,
                  External_Name => "glfwSetWindowTitle");

   procedure Get_Window_Pos (Window : System.Address;
                             Xpos, Ypos : out Windows.Coordinate);
   pragma Import (Convention => C, Entity => Get_Window_Pos,
                  External_Name => "glfwGetWindowPos");

   procedure Set_Window_Pos (Window : System.Address;
                             Xpos, Ypos : Windows.Coordinate);
   pragma Import (Convention => C, Entity => Set_Window_Pos,
                  External_Name => "glfwSetWindowPos");

   procedure Set_Window_Aspect_Ratio
     (Window : System.Address;
      Numer, Denom : Size)
   with Import, Convention => C, External_Name => "glfwSetWindowAspectRatio";

   procedure Set_Window_Size_Limits
     (Window : System.Address;
      Min_Width, Min_Height, Max_Width, Max_Height : Size)
   with Import, Convention => C, External_Name => "glfwSetWindowSizeLimits";

   procedure Get_Window_Content_Scale
     (Window : System.Address;
      X, Y   : out Interfaces.C.C_float)
   with Import, Convention => C, External_Name => "glfwGetWindowContentScale";

   function Get_Window_Opacity (Window : System.Address) return Windows.Opacity
     with Import, Convention => C, External_Name => "glfwGetWindowOpacity";

   procedure Set_Window_Opacity
     (Window  : System.Address;
      Opacity : Windows.Opacity)
   with Import, Convention => C, External_Name => "glfwSetWindowOpacity";

   procedure Get_Window_Size (Window : System.Address;
                              Width, Height : out Size);
   pragma Import (Convention => C, Entity => Get_Window_Size,
                  External_Name => "glfwGetWindowSize");

   procedure Get_Window_Frame_Size
     (Window : System.Address;
      Left, Top, Right, Bottom : out Size)
   with Import, Convention => C, External_Name => "glfwGetWindowFrameSize";

   procedure Set_Window_Size (Window : System.Address;
                              Width, Height : Size);
   pragma Import (Convention => C, Entity => Set_Window_Size,
                  External_Name => "glfwSetWindowSize");

   procedure Get_Framebuffer_Size (Window : System.Address;
                                   Width, Height : out Size);
   pragma Import (Convention => C, Entity => Get_Framebuffer_Size,
                  External_Name => "glfwGetFramebufferSize");

   procedure Iconify_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Iconify_Window,
                  External_Name => "glfwIconifyWindow");

   procedure Restore_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Restore_Window,
                  External_Name => "glfwRestoreWindow");

   procedure Show_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Show_Window,
                  External_Name => "glfwShowWindow");

   procedure Hide_Window (Window : System.Address);
   pragma Import (Convention => C, Entity => Hide_Window,
                  External_Name => "glfwHideWindow");

   function Get_Window_Monitor (Window : System.Address) return System.Address;
   pragma Import (Convention => C, Entity => Get_Window_Monitor,
                  External_Name => "glfwGetWindowMonitor");

   procedure Set_Window_Monitor
     (Window  : System.Address;
      Monitor : System.Address;
      X, Y, Width, Height, Refresh_Rate : C.int)
   with Import, Convention => C, External_Name => "glfwSetWindowMonitor";

   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info) return C.int;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info) return Bool;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.API_Kind;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.OpenGL_Profile_Kind;
   function Get_Window_Attrib (Window : System.Address;
                               Attrib : Enums.Window_Info)
                               return Windows.Context.Robustness_Kind;
   pragma Import (Convention => C, Entity => Get_Window_Attrib,
                  External_Name => "glfwGetWindowAttrib");

   procedure Set_Window_Attrib
     (Window : System.Address;
      Attrib : Enums.Window_Attrib_Setter;
      Enable : Bool)
   with Import, Convention => C, External_Name => "glfwSetWindowAttrib";

   procedure Set_Window_User_Pointer (Window  : System.Address;
                                      Pointer : System.Address);
   pragma Import (Convention => C, Entity => Set_Window_User_Pointer,
                  External_Name => "glfwSetWindowUserPointer");

   function Get_Window_User_Pointer (Window  : System.Address)
                                     return System.Address;
   pragma Import (Convention => C, Entity => Get_Window_User_Pointer,
                  External_Name => "glfwGetWindowUserPointer");

   --  The callback setters in the C header are defined as returning the
   --  previous callback pointer. This is rather low-level and not applicable
   --  in the object-oriented interface of this binding. So we define the setters
   --  as procedures, the return value will just get thrown away.

   procedure Set_Window_Pos_Callback (Window : System.Address;
                                     CB_Fun : Window_Position_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Pos_Callback,
                  External_Name => "glfwSetWindowPosCallback");

   procedure Set_Window_Size_Callback (Window : System.Address;
                                      CB_Fun : Window_Size_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Size_Callback,
                  External_Name => "glfwSetWindowSizeCallback");

   procedure Set_Window_Close_Callback (Window : System.Address;
                                       CB_Fun : Window_Close_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Close_Callback,
                  External_Name => "glfwSetWindowCloseCallback");

   procedure Set_Window_Refresh_Callback (Window : System.Address;
                                         CB_Fun : Window_Refresh_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Refresh_Callback,
                  External_Name => "glfwSetWindowRefreshCallback");

   procedure Set_Window_Focus_Callback (Window : System.Address;
                                       CB_Fun : Window_Focus_Callback);
   pragma Import (Convention => C, Entity => Set_Window_Focus_Callback,
                  External_Name => "glfwSetWindowFocusCallback");

   procedure Set_Window_Maximize_Callback
     (Window : System.Address;
      CB_Fun : Window_Maximize_Callback)
   with Import, Convention => C, External_Name => "glfwSetWindowMaximizeCallback";

   procedure Set_Window_Content_Scale_Callback
     (Window : System.Address;
      CB_Fun : Window_Content_Scale_Callback)
   with Import, Convention => C, External_Name => "glfwSetWindowContentScaleCallback";

   procedure Set_Window_Iconify_Callback (Window : System.Address;
                                         CB_Fun : Window_Iconify_Callback);
                                         --return Window_Iconify_Callback;
   pragma Import (Convention => C, Entity => Set_Window_Iconify_Callback,
                  External_Name => "glfwSetWindowIconifyCallback");

   procedure Set_Framebuffer_Size_Callback (Window : System.Address;
                                           CB_Fun : Framebuffer_Size_Callback);
                                           --return Framebuffer_Size_Callback;
   pragma Import (Convention => C, Entity => Set_Framebuffer_Size_Callback,
                  External_Name => "glfwSetFramebufferSizeCallback");

   -----------------------------------------------------------------------------
   --  Input
   -----------------------------------------------------------------------------

   function Get_Input_Mode (Window : System.Address;
                            Mode : Input.Input_Toggle) return Bool;
   function Get_Input_Mode (Window : System.Address;
                            Mode : Enums.Input_Toggle)
                            return Input.Mouse.Cursor_Mode;
   pragma Import (Convention => C, Entity => Get_Input_Mode,
                  External_Name => "glfwGetInputMode");

   procedure Set_Input_Mode (Window : System.Address;
                             Mode   : Input.Input_Toggle;
                             Value  : Bool);
   procedure Set_Input_Mode (Window : System.Address;
                             Mode   : Enums.Input_Toggle;
                             Value  : Input.Mouse.Cursor_Mode);
   pragma Import (Convention => C, Entity => Set_Input_Mode,
                  External_Name => "glfwSetInputMode");

   function Raw_Mouse_Motion_Supported return Bool
     with Import, Convention => C, External_Name => "glfwRawMouseMotionSupported";

   function Get_Key (Window : System.Address; Key : Input.Keys.Key)
                     return Input.Button_State;
   pragma Import (Convention => C, Entity => Get_Key,
                  External_Name => "glfwGetKey");

   function Get_Key_Name
     (Key  : Input.Keys.Key;
      Code : Input.Keys.Scancode) return C.Strings.chars_ptr
   with Import, Convention => C, External_Name => "glfwGetKeyName";

   function Get_Key_Code (Key : Input.Keys.Key) return Input.Keys.Scancode
     with Import, Convention => C, External_Name => "glfwGetKeyScancode";

   function Get_Mouse_Button (Window : System.Address;
                              Button : Input.Mouse.Button)
                              return Input.Button_State;
   pragma Import (Convention => C, Entity => Get_Mouse_Button,
                  External_Name => "glfwGetMouseButton");

   procedure Get_Cursor_Pos (Window : System.Address;
                             Xpos, Ypos : out Input.Mouse.Coordinate);
   pragma Import (Convention => C, Entity => Get_Cursor_Pos,
                  External_Name => "glfwGetCursorPos");

   procedure Set_Cursor_Pos (Window : System.Address;
                             Xpos, Ypos : Input.Mouse.Coordinate);
   pragma Import (Convention => C, Entity => Set_Cursor_Pos,
                  External_Name => "glfwSetCursorPos");

   procedure Set_Key_Callback (Window : System.Address;
                               CB_Fun : Key_Callback);
                               --return Key_Callback
   pragma Import (Convention => C, Entity => Set_Key_Callback,
                  External_Name => "glfwSetKeyCallback");

   procedure Set_Char_Callback (Window : System.Address;
                                CB_Fun : Character_Callback);
                                --return Character_Callback;
   pragma Import (Convention => C, Entity => Set_Char_Callback,
                  External_Name => "glfwSetCharCallback");

   procedure Set_Mouse_Button_Callback (Window : System.Address;
                                        CB_Fun : Mouse_Button_Callback);
                                        --return Mouse_Button_Callback;
   pragma Import (Convention => C, Entity => Set_Mouse_Button_Callback,
                  External_Name => "glfwSetMouseButtonCallback");

   procedure Set_Cursor_Pos_Callback (Window : System.Address;
                                      CB_Fun : Cursor_Position_Callback);
                                      --return Cursor_Position_Callback;
   pragma Import (Convention => C, Entity => Set_Cursor_Pos_Callback,
                  External_Name => "glfwSetCursorPosCallback");

   procedure Set_Cursor_Enter_Callback (Window : System.Address;
                                        CB_Fun : Cursor_Enter_Callback);
                                        --return Cursor_Enter_Callback;
   pragma Import (Convention => C, Entity => Set_Cursor_Enter_Callback,
                  External_Name => "glfwSetCursorEnterCallback");

   procedure Set_Scroll_Callback (Window : System.Address;
                                  CB_Fun : Scroll_Callback);
                                  --return Scroll_Callback;
   pragma Import (Convention => C, Entity => Set_Scroll_Callback,
                  External_Name => "glfwSetScrollCallback");

   function Joystick_Present (Joy : Enums.Joystick_ID) return Bool;
   pragma Import (Convention => C, Entity => Joystick_Present,
                  External_Name => "glfwJoystickPresent");

   function Joystick_Is_Gamepad (Joy : Enums.Joystick_ID) return Bool
     with Import, Convention => C, External_Name => "glfwJoystickIsGamepad";

   function Get_Joystick_GUID (Joy : Enums.Joystick_ID) return C.Strings.chars_ptr
     with Import, Convention => C, External_Name => "glfwGetJoystickGUID";

   function Get_Gamepad_Name (Joy : Enums.Joystick_ID) return C.Strings.chars_ptr
     with Import, Convention => C, External_Name => "glfwGetGamepadName";

   function Get_Gamepad_State
     (Joy   : Enums.Joystick_ID;
      State : out Input.Joysticks.Joystick_Gamepad_State) return Bool
   with Import, Convention => C, External_Name => "glfwGetGamepadState";

   function Update_Gamepad_Mappings (Mappings : C.Strings.chars_ptr) return Bool
     with Import, Convention => C, External_Name => "glfwUpdateGamepadMappings";

   function Get_Joystick_Axes (Joy : Enums.Joystick_ID;
                               Count : access Interfaces.C.int)
                               return Axis_Position_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Joystick_Axes,
                  External_Name => "glfwGetJoystickAxes");

   function Get_Joystick_Buttons (Joy : Enums.Joystick_ID;
                                  Count : access Interfaces.C.int)
                                  return Joystick_Button_State_List_Pointers.Pointer;
   pragma Import (Convention => C, Entity => Get_Joystick_Buttons,
                  External_Name => "glfwGetJoystickButtons");

   function Get_Joystick_Hats
     (Joy   : Enums.Joystick_ID;
      Count : out C.int) return Joystick_Hat_State_List_Pointers.Pointer
   with Import, Convention => C, External_Name => "glfwGetJoystickHats";

   function Get_Joystick_Name (Joy : Enums.Joystick_ID)
                               return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Joystick_Name,
                  External_Name => "glfwGetJoystickName");

   procedure Set_Joystick_Callback
     (CB_Fun : Joystick_Callback)
   with Import, Convention => C, External_Name => "glfwSetJoystickCallback";

   procedure Poll_Events;
   pragma Import (Convention => C, Entity => Poll_Events,
                  External_Name => "glfwPollEvents");

   procedure Wait_Events;
   pragma Import (Convention => C, Entity => Wait_Events,
                  External_Name => "glfwWaitEvents");

   procedure Wait_Events_Timeout (Timeout : C.double)
     with Import, Convention => C, External_Name => "glfwWaitEventsTimeout";

   procedure Post_Empty_Event
     with Import, Convention => C, External_Name => "glfwPostEmptyEvent";

   -----------------------------------------------------------------------------
   --  Context
   -----------------------------------------------------------------------------

   procedure Make_Context_Current (Window : System.Address);
   pragma Import (Convention => C, Entity => Make_Context_Current,
                  External_Name => "glfwMakeContextCurrent");

   function Get_Current_Context return System.Address;
   pragma Import (Convention => C, Entity => Get_Current_Context,
                  External_Name => "glfwGetCurrentContext");

   procedure Swap_Buffers (Window : System.Address);
   pragma Import (Convention => C, Entity => Swap_Buffers,
                  External_Name => "glfwSwapBuffers");

   procedure Swap_Interval (Value : Windows.Context.Swap_Interval);
   pragma Import (Convention => C, Entity => Swap_Interval,
                  External_Name => "glfwSwapInterval");

   -----------------------------------------------------------------------------
   --  Clipboard
   -----------------------------------------------------------------------------

   function Get_Clipboard_String (Window : System.Address)
                                  return Interfaces.C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Get_Clipboard_String,
                  External_Name => "glfwGetClipboardString");

   procedure Set_Clipboard_String (Window : System.Address;
                                   Value  : Interfaces.C.char_array);
   pragma Import (Convention => C, Entity => Set_Clipboard_String,
                  External_Name => "glfwSetClipboardString");

   -----------------------------------------------------------------------------
   --  File drop
   -----------------------------------------------------------------------------

   procedure Set_Drop_Callback
     (Window : System.Address;
      CB_Fun : File_Drop_Callback)
   with Import, Convention => C, External_Name => "glfwSetDropCallback";

end Glfw.API;
