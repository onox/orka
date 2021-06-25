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

private with Ada.Finalization;

package AWT.Monitors is
   pragma Preelaborate;

   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   type Monitor_State is record
      X, Y          : Integer  := 0;
      Width, Height : Natural  := 0;
      Refresh       : Duration := 0.0;
      Scale         : Positive := 1;
      Name          : SU.Unbounded_String;
   end record;

   type Monitor is limited interface;

   type Monitor_Ptr is not null access constant Monitor'Class;

   function Is_Connected (Object : Monitor) return Boolean is abstract;

   function ID (Object : Monitor) return Natural is abstract
     with Pre'Class => Object.Is_Connected;

   function State (Object : Monitor) return Monitor_State is abstract;

   procedure Log_Information (Monitor : AWT.Monitors.Monitor'Class);

   ----------------------------------------------------------------------------

   type Monitor_Array is array (Positive range <>) of Monitor_Ptr;

   function Monitors return Monitor_Array;

   ----------------------------------------------------------------------------

   type Monitor_Event_Listener is abstract tagged limited private;

   type Monitor_Event_Listener_Ptr is access constant Monitor_Event_Listener'Class;

   procedure On_Connect
     (Object  : Monitor_Event_Listener;
      Monitor : Monitor_Ptr) is abstract;

   procedure On_Disconnect
     (Object  : Monitor_Event_Listener;
      Monitor : Monitor_Ptr) is abstract;

private

   type Monitor_Event_Listener is
     abstract limited new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (Object : in out Monitor_Event_Listener);
   overriding procedure Finalize   (Object : in out Monitor_Event_Listener);

end AWT.Monitors;
