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

with AUnit.Test_Cases;

with Orka.Contexts.EGL;
with Orka.Resources.Locations;

generic
   with procedure Initialize_Shaders
     (Context                 : Orka.Contexts.Context_Access;
      Prefix_Sum, Tensors_GPU : Orka.Resources.Locations.Location_Ptr) is null;
package Test_Fixtures_GPU_Tensors is

   type Test_Case_With_Context is abstract new AUnit.Test_Cases.Test_Case with record
      Context : aliased Orka.Contexts.EGL.Device_EGL_Context := Orka.Contexts.EGL.Create_Context
        (Version => (4, 2),
         Flags   => (Debug => True, others => False));
   end record;

   overriding
   procedure Set_Up_Case (Object : in out Test_Case_With_Context);

end Test_Fixtures_GPU_Tensors;
