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

with Glfw.Windows.Context;

package Glfw.Windows.Hints is
   pragma Preelaborate;

   procedure Reset_To_Defaults;

   -----------------------------------------------------------------------------
   -- Window related
   -----------------------------------------------------------------------------

   procedure Set_Focused   (Value : Boolean);
   procedure Set_Floating  (Value : Boolean);
   procedure Set_Maximized (Value : Boolean);
   procedure Set_Resizable (Value : Boolean);
   procedure Set_Visible   (Value : Boolean);
   procedure Set_Decorated (Value : Boolean);
   procedure Set_Focus_On_Show (Value : Boolean);
   procedure Set_Auto_Iconify  (Value : Boolean);
   procedure Set_Center_On_Cursor (Value : Boolean);
   procedure Set_Scale_To_Monitor (Value : Boolean);
   procedure Set_Transparent_Framebuffer (Value : Boolean);

   -----------------------------------------------------------------------------
   -- Framebuffer related
   -----------------------------------------------------------------------------

   procedure Set_Color_Bits (Red, Green, Blue, Alpha : Natural);

   procedure Set_Depth_Bits   (Value : Natural);
   procedure Set_Stencil_Bits (Value : Natural);

   procedure Set_Stereo (Value : Boolean);

   procedure Set_Samples (Value : Natural);

   procedure Set_SRGB_Capable (Value : Boolean);

   procedure Set_Refresh_Rate (Value : Natural);

   -----------------------------------------------------------------------------
   -- Context related
   -----------------------------------------------------------------------------

   procedure Set_Client_API (Value : Context.API_Kind);

   procedure Set_Minimum_OpenGL_Version (Major : Positive; Minor : Natural);

   procedure Set_Robustness (Value : Context.Robustness_Kind);

   procedure Set_Forward_Compat (Value : Boolean);

   procedure Set_Debug_Context (Value : Boolean);

   procedure Set_Profile (Value : Context.OpenGL_Profile_Kind);

   procedure Set_Release_Behavior (Value : Context.Release_Behavior);

   procedure Set_No_Error_Context (Value : Boolean);

private

   --  To be able to use renames
   pragma Convention (C, Reset_To_Defaults);

end Glfw.Windows.Hints;
