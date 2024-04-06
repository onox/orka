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

private with GL.Objects.Pipelines;
private with GL.Objects.Vertex_Arrays;

private with Orka.Types;

private with EGL.Debug;
private with EGL.Errors;
private with EGL.Objects.Displays;
private with EGL.Objects.Contexts;

with EGL.Objects.Devices;

package Orka.Contexts.EGL is
   pragma Preelaborate;

   type Device_EGL_Context is limited new Context with private;

   overriding
   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Device_EGL_Context;
   --  Return a surfaceless EGL context using the default device

   function Create_Context
     (Device  : Standard.EGL.Objects.Devices.Device;
      Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Device_EGL_Context;
   --  Return a surfaceless EGL context using the given device

private

   type GL_Context_Type is record
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Pipeline     : GL.Objects.Pipelines.Pipeline;
   end record;

   package Optional_GL_Contexts is new Orka.Types.Optionals (GL_Context_Type);

   subtype Optional_GL_Context is Optional_GL_Contexts.Optional;

   type EGL_Context is abstract
     limited new Ada.Finalization.Limited_Controlled and Context with
   record
      Version : Context_Version;
      Flags   : Context_Flags;
      GL_Context     : Optional_GL_Context;
      Previous_State : Orka.Rendering.States.State;
   end record;

   overriding
   procedure Finalize (Object : in out EGL_Context);

   overriding
   function Version (Object : EGL_Context) return Context_Version is (Object.Version);

   overriding
   function Flags (Object : EGL_Context) return Context_Flags is (Object.Flags);

   overriding
   procedure Update_State (Object : in out EGL_Context; State : Orka.Rendering.States.State);

   overriding
   procedure Bind_Shaders (Object : EGL_Context; Stages : Orka.Rendering.Programs.Shaders.Shader_Programs);

   type Device_EGL_Context is limited new EGL_Context with record
      Context : Standard.EGL.Objects.Contexts.Context (Standard.EGL.Objects.Displays.Device);
   end record;

   overriding
   function Is_Current (Object : Device_EGL_Context; Kind : Task_Kind) return Boolean is
     (Object.Context.Is_Current
        (case Kind is
           when Current_Task => Standard.EGL.Objects.Contexts.Current_Task,
           when Any_Task     => Standard.EGL.Objects.Contexts.Any_Task));

   overriding
   procedure Make_Current (Object : Device_EGL_Context);

   overriding
   procedure Make_Not_Current (Object : Device_EGL_Context);

   ----------------------------------------------------------------------------

   procedure Print_Debug
     (Display : Standard.EGL.Objects.Displays.Display;
      Version : Context_Version;
      Flags   : Context_Flags);

   procedure Print_Error
     (Error : Standard.EGL.Errors.Error_Code;
      Level : Standard.EGL.Debug.Severity;
      Command, Message : String);

   procedure Post_Initialize (Object : in out EGL_Context'Class);

end Orka.Contexts.EGL;
