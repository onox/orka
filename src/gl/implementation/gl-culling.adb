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

package body GL.Culling is

   procedure Set_Front_Face (Face : Orientation) renames API.Front_Face;

   function Front_Face return Orientation is
      Ret : aliased Orientation;
   begin
      API.Get_Orientation (Enums.Getter.Cull_Face, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Front_Face;

   procedure Set_Cull_Face (Selector : Face_Selector) renames API.Cull_Face;

   function Cull_Face return Face_Selector is
      Ret : aliased Face_Selector;
   begin
      API.Get_Face_Selector (Enums.Getter.Cull_Face_Mode, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Cull_Face;

end GL.Culling;
