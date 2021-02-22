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

private with Ada.Finalization;

private with GL.Objects.Vertex_Arrays;

private with EGL.Objects.Displays;
private with EGL.Objects.Contexts;

with EGL.Objects.Devices;

package Orka.Contexts.EGL is

   type EGL_Context is limited new Context with private;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return EGL_Context;
   --  Return a surfaceless EGL context

   function Create_Context
     (Device  : Standard.EGL.Objects.Devices.Device;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return EGL_Context;
   --  Return a surfaceless EGL context using the given device

private

   type EGL_Context is
     limited new Ada.Finalization.Limited_Controlled and Context with
   record
      Version  : Context_Version;
      Flags    : Context_Flags;
      Features : Feature_Array := (others => False);
      Context  : Standard.EGL.Objects.Contexts.Context (Standard.EGL.Objects.Displays.Device);
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   end record;

   overriding
   procedure Finalize (Object : in out EGL_Context);

   overriding
   procedure Enable (Object : in out EGL_Context; Subject : Feature);

   overriding
   function Enabled (Object : EGL_Context; Subject : Feature) return Boolean;

end Orka.Contexts.EGL;
