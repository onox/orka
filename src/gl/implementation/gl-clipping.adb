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
with GL.Enums.Getter;

package body GL.Clipping is

   procedure Set_Clipping (Origin : Viewport_Origin; Depth : Depth_Mode) is
   begin
      API.Clip_Control (Origin, Depth);
      Raise_Exception_On_OpenGL_Error;
   end Set_Clipping;

   function Origin return Viewport_Origin is
      Result : Viewport_Origin := Viewport_Origin'First;
   begin
      API.Get_Clip_Origin (Enums.Getter.Clip_Origin, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Origin;

   function Depth return Depth_Mode is
      Result : Depth_Mode := Depth_Mode'First;
   begin
      API.Get_Clip_Depth_Mode (Enums.Getter.Clip_Depth_Mode, Result);
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Depth;

end GL.Clipping;
