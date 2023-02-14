--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with AWT.Registry;

package body AWT.Wayland is

   procedure Set_Callback (Callback : not null On_Available_Interface) is
   begin
      AWT.Registry.Registry_Callback := Callback;
   end Set_Callback;

   function Get_Display return not null access WP.Client.Display is
     (AWT.Registry.Global.Display'Access);

end AWT.Wayland;
