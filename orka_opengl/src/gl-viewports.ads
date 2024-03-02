--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

private with GL.Low_Level;

package GL.Viewports is
   pragma Preelaborate;

   use GL.Types;

   -----------------------------------------------------------------------------
   --                                Viewports                                --
   -----------------------------------------------------------------------------

   type Viewport is record
      X, Y, Width, Height : Single;
   end record;

   type Depth_Range is record
      Near, Far : Double;
   end record;

   type Scissor_Rectangle is record
      Left, Bottom  : Int;
      Width, Height : Size;
   end record;

   type Viewport_List is array (UInt range <>) of Viewport
     with Convention => C;

   type Depth_Range_List is array (UInt range <>) of Depth_Range
     with Convention => C;

   type Scissor_Rectangle_List is array (UInt range <>) of Scissor_Rectangle
     with Convention => C;

   procedure Set_Viewports (List : Viewport_List);

   procedure Set_Depth_Ranges (List : Depth_Range_List);

   procedure Set_Scissor_Rectangles (List : Scissor_Rectangle_List);

   -----------------------------------------------------------------------------
   --                                 Clipping                                --
   -----------------------------------------------------------------------------

   type Viewport_Origin is (Lower_Left, Upper_Left);

   type Depth_Mode is (Negative_One_To_One, Zero_To_One);

   procedure Set_Clipping (Origin : Viewport_Origin; Depth : Depth_Mode);
   --  Set the origin of the viewport and the range of the clip planes
   --
   --  Controls how clip space is mapped to window space. Both Direct3D and
   --  OpenGL expect a vertex position of (-1, -1) to map to the lower-left
   --  corner of the viewport.
   --
   --  Direct3D expects the UV coordinate of (0, 0) to correspond to the
   --  upper-left corner of a randered image, while OpenGL expects it in
   --  the lower-left corner.

private

   for Viewport_Origin use
     (Lower_Left => 16#8CA1#,
      Upper_Left => 16#8CA2#);
   for Viewport_Origin'Size use Low_Level.Enum'Size;

   for Depth_Mode use
     (Negative_One_To_One => 16#935E#,
      Zero_To_One         => 16#935F#);
   for Depth_Mode'Size use Low_Level.Enum'Size;

end GL.Viewports;
