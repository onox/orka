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

with Interfaces.C;

private package Glfw.Enums is
   pragma Preelaborate;

   type Window_Info is (Focused, Iconified, Resizable, Visible, Decorated,
                        Auto_Iconify, Floating, Maximized, Center_Cursor,
                        Transparent, Hovered, Focus_On_Show,
                        Red_Bits, Green_Bits, Blue_Bits, Alpha_Bits, Depth_Bits,
                        Stencil_Bits, Stereo, Samples, SRGB_Capable,
                        Refresh_Rate, Client_API, Context_Version_Major,
                        Context_Version_Minor, Context_Revision,
                        Context_Robustness, OpenGL_Forward_Compat,
                        OpenGL_Debug_Context, OpenGL_Profile,
                        Context_Rel_Behavior, Context_No_Error, Scale_To_Monitor);

   for Window_Info use (Focused       => 16#20001#,  --  h g
                        Iconified     => 16#20002#,  --    g
                        Resizable     => 16#20003#,  --  h g s
                        Visible       => 16#20004#,  --  h g
                        Decorated     => 16#20005#,  --  h g s
                        Auto_Iconify  => 16#20006#,  --  h g s
                        Floating      => 16#20007#,  --  h g s
                        Maximized     => 16#20008#,  --  h g
                        Center_Cursor => 16#20009#,  --  h
                        Transparent   => 16#2000A#,  --  h g
                        Hovered       => 16#2000B#,  --    g
                        Focus_On_Show => 16#2000C#,  --  h g s

                        Red_Bits      => 16#21001#,  --  h
                        Green_Bits    => 16#21002#,  --  h
                        Blue_Bits     => 16#21003#,  --  h
                        Alpha_Bits    => 16#21004#,  --  h
                        Depth_Bits    => 16#21005#,  --  h
                        Stencil_Bits  => 16#21006#,  --  h

                        Stereo        => 16#2100C#,  --  h
                        Samples       => 16#2100D#,  --  h
                        SRGB_Capable  => 16#2100E#,  --  h
                        Refresh_Rate  => 16#2100F#,  --  h

                        Client_API             => 16#22001#,  --  h g
                        Context_Version_Major  => 16#22002#,  --  h g
                        Context_Version_Minor  => 16#22003#,  --  h g
                        Context_Revision       => 16#22004#,  --    g
                        Context_Robustness     => 16#22005#,  --  h g
                        OpenGL_Forward_Compat  => 16#22006#,  --  h g
                        OpenGL_Debug_Context   => 16#22007#,  --  h g
                        OpenGL_Profile         => 16#22008#,  --  h g
                        Context_Rel_Behavior   => 16#22009#,  --  h
                        Context_No_Error       => 16#2200A#,  --  h
                        Scale_To_Monitor       => 16#2200C#); --  h
   for Window_Info'Size use Interfaces.C.int'Size;

   subtype Window_Hint is Window_Info
     with Static_Predicate => Window_Hint not in Iconified | Hovered | Context_Revision;

   subtype Window_Attrib_Getter is Window_Info
     with Static_Predicate => Window_Attrib_Getter
       in Focused .. Maximized | Transparent .. Focus_On_Show | Client_API .. OpenGL_Profile;

   subtype Window_Attrib_Setter is Window_Info
     with Static_Predicate => Window_Attrib_Setter
       in Resizable | Decorated | Auto_Iconify | Floating | Focus_On_Show;

   type Init_Hint is (Joystick_Hat_Buttons);

   type Input_Toggle is (Mouse_Cursor);

   type Joystick_ID is
     (Joystick_1, Joystick_2, Joystick_3, Joystick_4, Joystick_5,
      Joystick_6, Joystick_7, Joystick_8, Joystick_9, Joystick_10,
      Joystick_11, Joystick_12, Joystick_13, Joystick_14,
      Joystick_15, Joystick_16);

   type Joystick_Param is (Present, Axis, Buttons);

private

   for Init_Hint use
     (Joystick_Hat_Buttons => 16#50001#);
   for Init_Hint'Size use Interfaces.C.int'Size;

   for Input_Toggle use (Mouse_Cursor => 16#33001#);
   for Input_Toggle'Size use Interfaces.C.int'Size;

   for Joystick_ID use
     (Joystick_1  =>  0, Joystick_2  =>  1, Joystick_3  =>  2,
      Joystick_4  =>  3, Joystick_5  =>  4, Joystick_6  =>  5,
      Joystick_7  =>  6, Joystick_8  =>  7, Joystick_9  =>  8,
      Joystick_10 =>  9, Joystick_11 => 10, Joystick_12 => 11,
      Joystick_13 => 12, Joystick_14 => 13, Joystick_15 => 14,
      Joystick_16 => 15);
   for Joystick_ID'Size use Interfaces.C.int'Size;

   for Joystick_Param use (Present => 16#50001#,
                           Axis    => 16#50002#,
                           Buttons => 16#50003#);
   for Joystick_Param'Size use C.int'Size;

end Glfw.Enums;
