--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

package body GL is
   
   procedure Flush is
   begin
      API.Flush;
   end Flush;

   procedure Finish is
   begin
      API.Finish;
   end Finish;

   -- implementation depends on whether Auto_Exceptions has been enabled.
   procedure Raise_Exception_On_OpenGL_Error is separate;
end GL;
