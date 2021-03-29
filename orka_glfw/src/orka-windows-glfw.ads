--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

private with Ada.Finalization;

private with GL.Objects.Vertex_Arrays;
private with GL.Types;

private with Orka.Inputs.Pointers;

private with Glfw.Windows;
private with Glfw.Input.Keys;
private with Glfw.Input.Mouse;

with Orka.Contexts;

package Orka.Windows.GLFW is

   type GLFW_Context is limited new Contexts.Surface_Context with private;

   overriding
   function Create_Context
     (Version : Contexts.Context_Version;
      Flags   : Contexts.Context_Flags := (others => False)) return GLFW_Context;

   type GLFW_Window is limited new Window with private;

   overriding
   procedure Set_Title (Object : in out GLFW_Window; Value : String);

   overriding
   function Should_Close (Object : GLFW_Window) return Boolean;

   overriding
   function Create_Window
     (Context            : Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return GLFW_Window
   with Pre => Context in GLFW_Context'Class;

private

   type GLFW_Context is
     limited new Ada.Finalization.Limited_Controlled and Contexts.Surface_Context with
   record
      Version  : Contexts.Context_Version;
      Flags    : Contexts.Context_Flags;
      Features : Contexts.Feature_Array := (others => False);
   end record;

   overriding
   procedure Finalize (Object : in out GLFW_Context);

   overriding
   procedure Enable (Object : in out GLFW_Context; Subject : Contexts.Feature);

   overriding
   function Enabled (Object : GLFW_Context; Subject : Contexts.Feature) return Boolean;

   overriding
   function Version (Object : GLFW_Context) return Contexts.Context_Version;

   overriding
   function Flags (Object : GLFW_Context) return Contexts.Context_Flags;

   overriding
   function Is_Current (Object : GLFW_Context) return Boolean;

   overriding
   procedure Make_Current (Object : GLFW_Context);

   overriding
   procedure Make_Current
     (Object : GLFW_Context;
      Window : in out Orka.Windows.Window'Class);

   overriding
   procedure Make_Not_Current (Object : GLFW_Context);

   type GLFW_Window is limited new Standard.Glfw.Windows.Window and Window with record
      Input     : Inputs.Pointers.Pointer_Input_Ptr;
      Finalized : Boolean;

      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

      Position_X : GL.Types.Double := 0.0;
      Position_Y : GL.Types.Double := 0.0;
      Scroll_X   : GL.Types.Double := 0.0;
      Scroll_Y   : GL.Types.Double := 0.0;
      Width, Height : Positive;

      --  Needed to workaround a GLFW bug
      Got_Locked, Last_Locked : Boolean := False;
   end record;

   overriding
   function Pointer_Input
     (Object : GLFW_Window) return Inputs.Pointers.Pointer_Input_Ptr;

   overriding
   function Width (Object : GLFW_Window) return Positive;

   overriding
   function Height (Object : GLFW_Window) return Positive;

   overriding
   procedure Close (Object : in out GLFW_Window);

   overriding
   procedure Process_Input (Object : in out GLFW_Window);

   overriding
   procedure Swap_Buffers (Object : in out GLFW_Window);

   overriding
   procedure Set_Vertical_Sync (Object : in out GLFW_Window; Enable : Boolean);

   overriding
   procedure Finalize (Object : in out GLFW_Window);

   overriding
   procedure Close_Requested (Object : not null access GLFW_Window);

   overriding
   procedure Key_Changed
     (Object   : not null access GLFW_Window;
      Key      : Standard.Glfw.Input.Keys.Key;
      Scancode : Standard.Glfw.Input.Keys.Scancode;
      Action   : Standard.Glfw.Input.Keys.Action;
      Mods     : Standard.Glfw.Input.Keys.Modifiers);

   overriding
   procedure Mouse_Position_Changed
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Coordinate);

   overriding
   procedure Mouse_Scrolled
     (Object : not null access GLFW_Window;
      X, Y   : Standard.Glfw.Input.Mouse.Scroll_Offset);

   overriding
   procedure Mouse_Button_Changed
     (Object  : not null access GLFW_Window;
      Button  : Standard.Glfw.Input.Mouse.Button;
      State   : Standard.Glfw.Input.Button_State;
      Mods    : Standard.Glfw.Input.Keys.Modifiers);

   overriding
   procedure Framebuffer_Size_Changed
     (Object : not null access GLFW_Window;
      Width, Height : Natural);

end Orka.Windows.GLFW;
