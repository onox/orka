--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Orka.Windows;

package Orka.Contexts is
   pragma Preelaborate;

   type Library (Debug : Boolean) is abstract tagged limited private;

   procedure Shutdown (Object : in out Library) is null;

   function Create_Window
     (Object : Library;
      Width, Height : Positive;
      Samples : Natural := 0;
      Visible, Resizable : Boolean := True) return Orka.Windows.Window'Class is abstract;

   -----------------------------------------------------------------------------

   type Feature is (Reversed_Z, Multisample, Sample_Shading);

   type Context is abstract tagged limited private;

   procedure Make_Current (Object : in out Context; Current : Boolean) is abstract;

   procedure Enable (Object : in out Context; Subject : Feature);
   --  Note: If enabling Reversed_Z, the depth must be cleared with the
   --  value 0.0

   function Enabled (Object : Context; Subject : Feature) return Boolean;

private

   type Library (Debug : Boolean) is abstract limited
     new Ada.Finalization.Limited_Controlled with
   record
      Finalized : Boolean := False;
   end record;

   overriding
   procedure Finalize (Object : in out Library);

   type Feature_Array is array (Feature) of Boolean;

   type Context is abstract limited
     new Ada.Finalization.Limited_Controlled with
   record
      Vertex_Array : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      Finalized : Boolean := False;
      Features  : Feature_Array := (others => False);
   end record;

   overriding
   procedure Initialize (Object : in out Context);

   overriding
   procedure Finalize (Object : in out Context);

end Orka.Contexts;
