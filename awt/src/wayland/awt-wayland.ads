--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Wayland.Enums;
with Wayland.Protocols.Client;

package AWT.Wayland is
   pragma Preelaborate;

   package WP renames Standard.Wayland.Protocols;
   package WE renames Standard.Wayland.Enums;

   type On_Available_Interface is access procedure
     (Registry : in out WP.Client.Registry'Class;
      Id       : Standard.Wayland.Unsigned_32;
      Name     : String;
      Version  : Standard.Wayland.Unsigned_32);

   procedure Set_Callback (Callback : not null On_Available_Interface);
   --  Set a callback that will be called when unknown interfaces are
   --  made available to the registry when AWT is being initialized
   --
   --  This gives applications a chance to bind objects for interfaces
   --  with which AWT is not familiar. The callback must be set *before*
   --  the procedure AWT.Initialize is executed.
   --
   --  The registry must not be destroyed by the callback or else a
   --  Program_Error will be raised.

   ----------------------------------------------------------------------------
   --                          Internal Subprograms                          --
   ----------------------------------------------------------------------------

   function Get_Display return not null access WP.Client.Display;
   --  Used by function Create_Context in Orka.Contexts.EGL.AWT to create an
   --  EGL context using the Wayland platform

end AWT.Wayland;
