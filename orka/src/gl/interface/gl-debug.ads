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

with Ada.Finalization;

with GL.Objects;
with GL.Types;

with Orka;

private with GL.Low_Level;

package GL.Debug is
   pragma Preelaborate;

   use GL.Types;

   type Source is (OpenGL, Window_System, Shader_Compiler,
                   Third_Party, Application, Other);

   type Message_Type is (Error, Deprecated_Behavior, Undefined_Behavior,
                         Portability, Performance, Other, Marker,
                         Push_Group, Pop_Group);

   type Severity is (Notification, High, Medium, Low);

   type Callback_Reference is access procedure
     (From      : Source;
      Kind      : Message_Type;
      Level     : Severity;
      ID        : UInt;
      Message   : String);
   --  By default Debug_Output_Synchronous is disabled (see GL.Toggles).
   --  The graphics driver may call the procedure from multiple tasks,
   --  concurrently, and/or asynchronously after executing an OpenGL command.

   procedure Set_Message_Callback (Callback : not null Callback_Reference);
   --  Set a message callback in order to receive debug messages
   --
   --  Generated messages will no longer be appended to the message log,
   --  but instead passed to the given callback. Initially, messages with
   --  severity Low are not enabled.

   procedure Disable_Message_Callback;
   --  Clear current message callback and disable message output
   --
   --  After having called this procedure, generated messages will instead
   --  be added to the message log.

   procedure Set (From : Source; Kind : Message_Type; Level : Severity;
                  Enabled : Boolean);
   --  Enable or disable specific messages or groups of messages

   procedure Set (Level : Severity; Enabled : Boolean);
   --  Enable messages that have the given severity

   procedure Set (From : Source; Kind : Message_Type; Identifiers : Orka.Unsigned_32_Array;
                  Enabled : Boolean)
     with Pre => Identifiers'Length > 0;
   --  Enable or disable messages that have one of the given message IDs

   procedure Insert_Message (From : Source; Kind : Message_Type; Level : Severity;
                             Identifier : UInt; Message : String);
   --  Generate a new debug message
   --
   --  From must be either Third_Party or Application. Instantiate the
   --  generic package Messages below if you need to print multiple
   --  messages with the same source and message type.
   --
   --  The generated debug message will either be passed to the callback
   --  (if there is one), or added to the message log (if not full).

   type Active_Group is limited new Ada.Finalization.Limited_Controlled with private;

   function Push_Debug_Group (From : Source; Identifier : UInt; Message : String)
     return Active_Group'Class;
   --  Add a new debug group to the stack
   --
   --  From must be either Third_Party or Application.
   --
   --  The value returned is of a controlled type. This means you must
   --  assign it to some local variable, so that the debug group will be
   --  automatically removed when the variable goes out of scope.
   --
   --  When the debug group is pushed onto the stack, a message
   --  is generated with the type Push_Group and severity Notification.
   --
   --  Any further calls to Set will only apply to the active debug
   --  group. The currently active debug group inherits the message
   --  filtering from the previous active group.
   --
   --  When the debug group is popped off the stack, a message is
   --  generated with type Pop_Group and severity Notification.

   procedure Annotate (Object : GL.Objects.GL_Object'Class; Message : String);
   --  Attach a label to the given object in order for the debug
   --  output to describe the object

   function Get_Label (Object : GL.Objects.GL_Object'Class) return String;
   --  Return the label attached to the given object

   function Max_Message_Length return Size
     with Post => Max_Message_Length'Result >= 1;

   generic
      From : Source;
      Kind : Message_Type;
      ID   : UInt := 0;
   procedure Log (Level : Severity; Message : String);
   --  Generate a new debug message

private

   for Source use
     (OpenGL          => 16#8246#,
      Window_System   => 16#8247#,
      Shader_Compiler => 16#8248#,
      Third_Party     => 16#8249#,
      Application     => 16#824A#,
      Other           => 16#824B#);
   for Source'Size use Low_Level.Enum'Size;

   for Message_Type use
     (Error               => 16#824C#,
      Deprecated_Behavior => 16#824D#,
      Undefined_Behavior  => 16#824E#,
      Portability         => 16#824F#,
      Performance         => 16#8250#,
      Other               => 16#8251#,
      Marker              => 16#8268#,
      Push_Group          => 16#8269#,
      Pop_Group           => 16#826A#);
   for Message_Type'Size use Low_Level.Enum'Size;

   for Severity use
     (Notification => 16#826B#,
      High         => 16#9146#,
      Medium       => 16#9147#,
      Low          => 16#9148#);
   for Severity'Size use Low_Level.Enum'Size;

   type Active_Group is limited new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Active_Group);

end GL.Debug;
