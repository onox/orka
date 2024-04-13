--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with Orka.Numerics.Singles.Tensors.GPU;

with Generic_Test_Tensors_Vectors;
with Test_Fixtures_GPU_Tensors_Singles;

package Test_Tensors_GPU_Singles_Vectors is new Generic_Test_Tensors_Vectors
  ("GPU - Singles",
   True,
   Test_Fixtures_GPU_Tensors_Singles.Test_Case_With_Context,
   Orka.Numerics.Singles.Tensors,
   Orka.Numerics.Singles.Tensors.GPU.GPU_Tensor,
   Orka.Numerics.Singles.Tensors.GPU.Reset_Random);
