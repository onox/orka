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

with Orka.Contexts;

package Orka.Windows.GLFW is

   type GLFW_Context is limited new Contexts.Surface_Context with private;

   overriding
   function Create_Context
     (Version : Contexts.Context_Version;
      Flags   : Contexts.Context_Flags := (others => False)) return GLFW_Context;

private

   type GLFW_Context is
     limited new Ada.Finalization.Limited_Controlled and Contexts.Surface_Context with
   record
      Flags    : Contexts.Context_Flags;
      Features : Contexts.Feature_Array := (others => False);
   end record;

   overriding
   procedure Finalize (Object : in out GLFW_Context);

   overriding
   function Create_Window
     (Object             : GLFW_Context;
      Width, Height      : Positive;
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True) return Windows.Window'Class;

   overriding
   procedure Enable (Object : in out GLFW_Context; Subject : Contexts.Feature);

   overriding
   function Enabled (Object : GLFW_Context; Subject : Contexts.Feature) return Boolean;

end Orka.Windows.GLFW;
