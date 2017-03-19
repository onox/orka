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

package Orka.Behaviors is
   pragma Preelaborate;

   type Visibility is (Invisible, Visible);

   type Behavior is limited interface;

   type Behavior_Ptr is not null access Behavior'Class;

   procedure Fixed_Update (Object : in out Behavior; Delta_Time : Duration) is null;
   --  Called zero to multiple times per frame and before Update
   --
   --  Fixed_Update can be used to perform physics or other computations
   --  that require a stable Delta_Time that does not depend on the frame
   --  rate. If the game needs to catch up then Fixed_Update is called
   --  multiple times. If the game renders at a very high frame rate then
   --  Fixed_Update will not be called at all.

   procedure Update (Object : in out Behavior; Delta_Time : Duration) is null;
   --  Called once per frame and after any calls to Fixed_Update

   procedure After_Update (Object : in out Behavior; Delta_Time : Duration) is null;
   --  Called once per frame and after Update

   procedure Visibility_Changed (Object : in out Behavior; State : Visibility) is null;

   procedure Render (Object : in out Behavior) is null;

end Orka.Behaviors;
