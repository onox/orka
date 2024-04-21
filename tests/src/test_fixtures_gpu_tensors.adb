--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Orka.Resources.Locations.Directories;

package body Test_Fixtures_GPU_Tensors is

   overriding
   procedure Set_Up_Case (Object : in out Test_Case_With_Context) is
      use Orka.Resources.Locations;
   begin
      Register ("orka", Directories.Create_Location ("../orka/data/shaders"));
      Register ("orka-tensors-gpu", Directories.Create_Location ("../orka_tensors_gpu/data/shaders"));
      Initialize_Shaders (Object.Context'Unchecked_Access);
   end Set_Up_Case;

end Test_Fixtures_GPU_Tensors;
