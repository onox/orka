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

with GL.API;

package body GL.Viewports is

   -----------------------------------------------------------------------------
   --                                Viewports                                --
   -----------------------------------------------------------------------------

   procedure Set_Viewports (List : Viewport_List) is
   begin
      API.Viewport_Array.Ref (List'First, List'Length, List);
   end Set_Viewports;

   procedure Set_Depth_Ranges (List : Depth_Range_List) is
   begin
      API.Depth_Range_Array.Ref (List'First, List'Length, List);
   end Set_Depth_Ranges;

   procedure Set_Scissor_Rectangles (List : Scissor_Rectangle_List) is
   begin
      API.Scissor_Array.Ref (List'First, List'Length, List);
   end Set_Scissor_Rectangles;

   -----------------------------------------------------------------------------
   --                                 Clipping                                --
   -----------------------------------------------------------------------------

   procedure Set_Clipping (Origin : Viewport_Origin; Depth : Depth_Mode) is
   begin
      API.Clip_Control.Ref (Origin, Depth);
   end Set_Clipping;

end GL.Viewports;
