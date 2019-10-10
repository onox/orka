--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

package body Orka_Package_glTF is

   overriding
   procedure Execute
     (Object  : Create_Group_Job;
      Enqueue : not null access procedure (Element : Orka.Jobs.Job_Ptr)) is
   begin
      Object.Group.all := Object.Model.Create_Group (Object.Culler, Capacity => 1);
   end Execute;

   overriding
   procedure After_Update
     (Object        : in out No_Behavior;
      Delta_Time    : Duration;
      View_Position : Orka.Behaviors.Transforms.Vector4) is
   begin
      Object.Update_Transforms (View_Position);
   end After_Update;

end Orka_Package_glTF;
