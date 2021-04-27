--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

private with SDL.Video.GL;
private with SDL.Video.Windows;

with Orka.Contexts;

package Orka.Windows.SDL is
   pragma Preelaborate;

   type SDL_Context is limited new Contexts.Surface_Context with private;

   overriding
   function Create_Context
     (Version : Contexts.Context_Version;
      Flags   : Contexts.Context_Flags := (others => False)) return SDL_Context;

   type SDL_Window is limited new Window with private;

   overriding
   function Create_Window
     (Context            : Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return SDL_Window
   with Pre => Context in SDL_Context'Class;

   overriding
   function Pointer_Input
     (Object : SDL_Window) return Inputs.Pointers.Pointer_Input_Ptr;

   overriding
   function Width (Object : SDL_Window) return Positive;

   overriding
   function Height (Object : SDL_Window) return Positive;

   overriding
   procedure Set_Title (Object : in out SDL_Window; Value : String);

   overriding
   procedure Close (Object : in out SDL_Window);

   overriding
   function Should_Close (Object : SDL_Window) return Boolean;

   overriding
   procedure Process_Input (Object : in out SDL_Window);

   overriding
   procedure Swap_Buffers (Object : in out SDL_Window);

   overriding
   procedure Set_Vertical_Sync (Object : in out SDL_Window; Enable : Boolean);

private

   type SDL_Context is
     limited new Ada.Finalization.Limited_Controlled and Contexts.Surface_Context with
   record
      Version  : Contexts.Context_Version;
      Flags    : Contexts.Context_Flags;
      Features : Contexts.Feature_Array := (others => False);
   end record;

   overriding
   procedure Finalize (Object : in out SDL_Context);

   overriding
   procedure Enable (Object : in out SDL_Context; Subject : Contexts.Feature);

   overriding
   function Enabled (Object : SDL_Context; Subject : Contexts.Feature) return Boolean;

   overriding
   function Version (Object : SDL_Context) return Contexts.Context_Version;

   overriding
   function Flags (Object : SDL_Context) return Contexts.Context_Flags;

   overriding
   function Is_Current (Object : SDL_Context; Kind : Orka.Contexts.Task_Kind) return Boolean;

   overriding
   procedure Make_Current (Object : SDL_Context);

   overriding
   procedure Make_Current
     (Object : SDL_Context;
      Window : in out Orka.Windows.Window'Class);

   overriding
   procedure Make_Not_Current (Object : SDL_Context);

   type SDL_Window is limited new Ada.Finalization.Limited_Controlled and Window with record
      Input     : Inputs.Pointers.Pointer_Input_Ptr;
      Finalized : Boolean;

      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

      Context   : Standard.SDL.Video.GL.Contexts;
      Window    : Standard.SDL.Video.Windows.Window;

      Close_Window : Boolean := False;
      Position_X : GL.Types.Double := 0.0;
      Position_Y : GL.Types.Double := 0.0;
      Scroll_X   : GL.Types.Double := 0.0;
      Scroll_Y   : GL.Types.Double := 0.0;
      Width, Height : Positive;
   end record;

   overriding
   procedure Finalize (Object : in out SDL_Window);

end Orka.Windows.SDL;
