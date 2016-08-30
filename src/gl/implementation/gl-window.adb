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

with GL.API;
with GL.Enums.Getter;

package body GL.Window is

   procedure Set_Viewport (X, Y : Int; Width, Height : Size) is
   begin
      GL.API.Viewport (X, Y, Width, Height);
      Raise_Exception_On_OpenGL_Error;
   end Set_Viewport;

   procedure Get_Viewport (X, Y : out Int; Width, Height : out Size) is
      Ret : Ints.Vector4;
   begin
      API.Get_Int_Vec4 (Enums.Getter.Viewport, Ret);
      Raise_Exception_On_OpenGL_Error;
      X := Ret (GL.X);
      Y := Ret (GL.Y);
      Width := Size (Ret (Z));
      Height := Size (Ret (W));
   end Get_Viewport;

   procedure Set_Depth_Range (Near, Far : Double) is
   begin
      API.Depth_Range (Near, Far);
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Range;

   procedure Get_Depth_Range (Near, Far : out Double) is
      Ret : Doubles.Vector2;
   begin
      API.Get_Double_Vec2 (Enums.Getter.Depth_Range, Ret);
      Raise_Exception_On_OpenGL_Error;
      Near := Ret (X);
      Far := Ret (Y);
   end Get_Depth_Range;

end GL.Window;
