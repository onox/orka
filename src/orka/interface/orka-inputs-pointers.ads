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

with GL.Types;

package Orka.Inputs.Pointers is
   pragma Preelaborate;

   type Button is (Left, Right, Middle);

   type Button_State is (Released, Pressed);

   type Pointer_Input is interface;

   type Pointer_Input_Ptr is not null access Pointer_Input'Class;

   function Position_X (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  The X position of the pointer. Does not change while the pointer
   --  is locked.

   function Position_Y (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  The Y position of the pointer. Does not change while the pointer
   --  is locked.

   function Delta_X (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  Pointer movement in the X direction since previous frame. May
   --  change irrespective of whether pointer is locked or not.

   function Delta_Y (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  Pointer movement in the Y direction since previous frame. May
   --  change irrespective of whether pointer is locked or not.

   function Scroll_X (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  Scroll movement of pointer in the X direction since the previous frame

   function Scroll_Y (Object : Pointer_Input) return GL.Types.Single is abstract;
   --  Scroll movement of pointer in the Y direction since the previous frame

   function Locked (Object : Pointer_Input) return Boolean is abstract;

   function Visible (Object : Pointer_Input) return Boolean is abstract;

   procedure Lock_Pointer (Object : in out Pointer_Input; Locked : Boolean) is abstract;
   --  Lock the pointer to the current position
   --
   --  The position of the pointer when queried via Position_X / Position_Y
   --  will no longer change, but the movement relative to the previous
   --  frame (Delta_X / Delta_Y) still can. The delta movement can be used
   --  to control a camera using the pointer.

   procedure Set_Visible (Object : in out Pointer_Input; Visible : Boolean) is abstract;

   function Button_Pressed
     (Object  : Pointer_Input;
      Subject : Button) return Boolean is abstract;

end Orka.Inputs.Pointers;
