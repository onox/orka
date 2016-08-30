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

package body GL.Toggles is
   procedure Enable (Subject : Toggle) is
   begin
      API.Enable (Subject);
   end Enable;
   
   procedure Disable (Subject : Toggle) is
   begin
      API.Disable (Subject);
   end Disable;
   
   procedure Set (Subject : Toggle; Value : Toggle_State) is
   begin
      if Value = Disabled then
         API.Disable (Subject);
      else
         API.Enable (Subject);
      end if;
   end Set;
   
   function State (Subject : Toggle) return Toggle_State is
   begin
      if API.Is_Enabled (Subject) then
         return Enabled;
      else
         return Disabled;
      end if;
   end State;
end GL.Toggles;
