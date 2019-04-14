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

with GL.Types;

package GL.Window is
   pragma Preelaborate;

   use GL.Types;

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

   function Maximum_Viewports return Size
     with Post => Maximum_Viewports'Result >= 16;

   function Viewport_Subpixel_Bits return Size;

   function Origin_Range return Singles.Vector2;
   --  Return the minimum and maximum X and Y of the origin (lower left
   --  corner) of a viewport

   function Maximum_Extent return Singles.Vector2;
   --  Return the maximum width and height of a viewport

   procedure Set_Viewports (List : Viewport_List);
   function Get_Viewport (Index : UInt) return Viewport;

   procedure Set_Depth_Ranges (List : Depth_Range_List);
   function Get_Depth_Range (Index : UInt) return Depth_Range;

   procedure Set_Scissor_Rectangles (List : Scissor_Rectangle_List);
   function Get_Scissor_Rectangle (Index : UInt) return Scissor_Rectangle;

end GL.Window;
