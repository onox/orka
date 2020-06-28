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

with Interfaces.C.Strings;

with System;

with EGL.Errors;
with EGL.Loading;
with EGL.Objects.Contexts;
with EGL.Objects.Displays;
with EGL.Objects.Devices;

private package EGL.API is
   pragma Preelaborate;

   function Get_Proc_Address (Name : C.char_array) return System.Address
     with Import, Convention => C, External_Name => "eglGetProcAddress";

   function Get_Error return Errors.Error_Code
     with Import, Convention => C, External_Name => "eglGetError";

   package Debug_Message_Control is new Loading.Function_With_2_Params
     ("eglDebugMessageControlKHR", System.Address, Attribute_Array, Errors.Error_Code);

   -----------------------------------------------------------------------------
   --                                 Displays                                --
   -----------------------------------------------------------------------------

   package Get_Platform_Display is new Loading.Function_With_3_Params
     ("eglGetPlatformDisplayEXT", Objects.Displays.Platform_Kind, Void_Ptr, Int_Array, ID_Type);

   function Initialize_Display
     (Display      : ID_Type;
      Major, Minor : out Int) return Bool
   with Import, Convention => C, External_Name => "eglInitialize";

   function Terminate_Display (Display : ID_Type) return Bool
     with Import, Convention => C, External_Name => "eglTerminate";

   function Query_String
     (No_Display : ID_Type;
      Name       : Display_Query_Param) return C.Strings.chars_ptr
   with Import, Convention => C, External_Name => "eglQueryString";
   --  Return a zero-terminated string with the properties of the EGL
   --  client or the EGL display, or null if an error occurred

   -----------------------------------------------------------------------------
   --                                 Contexts                                --
   -----------------------------------------------------------------------------

   function Bind_API (API : Objects.Contexts.Client_API) return Bool
     with Import, Convention => C, External_Name => "eglBindAPI";

   function Create_Context
     (Display : ID_Type;
      Config  : ID_Type;
      Share   : ID_Type;
      Attributes : Int_Array) return ID_Type
   with Import, Convention => C, External_Name => "eglCreateContext";

   function Destroy_Context
     (Display : ID_Type;
      Context : ID_Type) return Bool
   with Import, Convention => C, External_Name => "eglDestroyContext";

   function Make_Current
     (Display : ID_Type;
      Draw    : ID_Type;
      Read    : ID_Type;
      Context : ID_Type) return Bool
   with Import, Convention => C, External_Name => "eglMakeCurrent";

   -----------------------------------------------------------------------------
   --                                 Surfaces                                --
   -----------------------------------------------------------------------------

   --  eglQueryContext to check if double buffered window is used
   --  eglChooseConfig or eglGetConfigs
   --  eglCreatePlatformWindowSurface[EXT]
   --  eglDestroySurface
   --  eglQuerySurface
   --  eglSurfaceAttrib

   function Swap_Buffers
     (Display : ID_Type;
      Surface : ID_Type) return Bool
   with Import, Convention => C, External_Name => "eglSwapBuffers";

   function Swap_Interval
     (Display  : ID_Type;
      Interval : Int) return Bool
   with Import, Convention => C, External_Name => "eglSwapInterval";

   -----------------------------------------------------------------------------
   --                                 Devices                                 --
   -----------------------------------------------------------------------------

   package Query_Device_String is new Loading.Function_With_2_Params
     ("eglQueryDeviceStringEXT", ID_Type, Device_Query_Param, C.Strings.chars_ptr);
   --  Return the DRM name of a device or a list of device extensions

   package Query_Display_Attrib is new Loading.Getter_With_3_Params
     ("eglQueryDisplayAttribEXT", ID_Type, Int, ID_Type, Bool);
   --  Return the device of a display

   package Query_Devices is new Loading.Array_Getter_With_3_Params
     ("eglQueryDevicesEXT", Int, ID_Type, ID_Array, Int, Bool);

end EGL.API;