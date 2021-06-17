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

with Ada.Containers.Vectors;

with Event_Device.Force_Feedbacks;

private package AWT.Gamepads is
   pragma Preelaborate;

   package ED renames Event_Device;
   package FF renames ED.Force_Feedbacks;

   package Effect_Vectors is new Ada.Containers.Vectors
     (Positive, FF.Force_Feedback_Effect, FF."=");

   type Uploaded_Effect is record
      Stop_At : Duration;
      Cursor  : Effect_Vectors.Cursor;
      Effect  : FF.Force_Feedback_Effect;
   end record;

   package Uploaded_Effect_Vectors is new Ada.Containers.Vectors (Positive, Uploaded_Effect);

   subtype Input_Axis is ED.Absolute_Axis_Kind;

   subtype Input_Hat is ED.Absolute_Axis_Kind range ED.Hat_0X .. ED.Hat_3Y;

   subtype Input_Button is ED.Key_Kind;

   type Abstract_Gamepad is abstract tagged limited record
      Device  : ED.Input_Device;
      Data    : ED.State;
      Path    : SU.Unbounded_String;
      Effects : Uploaded_Effect_Vectors.Vector;

      Battery : SU.Unbounded_String;

      LED_Red   : SU.Unbounded_String;
      LED_Green : SU.Unbounded_String;
      LED_Blue  : SU.Unbounded_String;
   end record;

   type Abstract_Effect is record
      Cursor : Effect_Vectors.Cursor;
   end record;

end AWT.Gamepads;
