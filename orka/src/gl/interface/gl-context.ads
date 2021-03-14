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

with Ada.Strings.Unbounded;

private with GL.Low_Level;

package GL.Context is
   pragma Preelaborate;

   procedure Flush;

   type String_List is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   type Reset_Status is (No_Error, Guilty, Innocent, Unknown);

   function Status return Reset_Status;
   --  Return the reset status of the context
   --
   --  If the context has been created with the Robust context flag
   --  enabled, then any OpenGL function may raise a Context_Lost_Error
   --  when a reset has occurred.
   --
   --  If a context has been lost, it must be recreated, including any
   --  OpenGL objects. This function will return a status not equal to
   --  No_Error while the context is still resetting and No_Error when
   --  the reset has been completed.

   type Context_Flags is record
      Forward_Compatible : Boolean := False;
      Debug              : Boolean := False;
      Robust_Access      : Boolean := False;
      No_Error           : Boolean := False;
   end record;

   function Flags return Context_Flags;

   type Context_Reset_Notification is (Lose_Context_On_Reset, No_Reset_Notification);

   function Reset_Notification return Context_Reset_Notification;

   type Context_Release_Behavior is (None, Flush);

   function Release_Behavior return Context_Release_Behavior;

   function Major_Version return Natural;
   function Minor_Version return Natural;
   --  These two require OpenGL 3

   function Version_String return String;
   --  Legacy (deprecated in OpenGL 3)

   function Vendor return String;
   function Renderer return String;

   function Extensions return String_List;
   function Has_Extension
     (Extensions : String_List;
      Name       : String) return Boolean;
   --  Uses OpenGL 3 interface

   function Primary_Shading_Language_Version return String;

   function Supported_Shading_Language_Versions return String_List;
   function Supports_Shading_Language_Version
     (Versions : String_List;
      Name     : String) return Boolean;
   --  Available since OpenGL 4.3

private

   for Reset_Status use
     (No_Error => 0,
      Guilty   => 16#8253#,
      Innocent => 16#8254#,
      Unknown  => 16#8255#);
   for Reset_Status'Size use Low_Level.Enum'Size;

   for Context_Flags use record
      Forward_Compatible at 0 range 0 .. 0;
      Debug              at 0 range 1 .. 1;
      Robust_Access      at 0 range 2 .. 2;
      No_Error           at 0 range 3 .. 3;
   end record;
   for Context_Flags'Size use Low_Level.Bitfield'Size;

   for Context_Reset_Notification use
     (Lose_Context_On_Reset => 16#8252#,
      No_Reset_Notification => 16#8261#);
   for Context_Reset_Notification'Size use Low_Level.Enum'Size;

   for Context_Release_Behavior use
     (None  => 0,
      Flush => 16#82FC#);
   for Context_Release_Behavior'Size use Low_Level.Enum'Size;

end GL.Context;
