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
   use GL.Types;

   procedure Set_Viewport (X, Y : Int; Width, Height : Size);
   procedure Get_Viewport (X, Y : out Int; Width, Height : out Size);

   procedure Set_Depth_Range (Near, Far : Double);
   procedure Get_Depth_Range (Near, Far : out Double);

end GL.Window;
