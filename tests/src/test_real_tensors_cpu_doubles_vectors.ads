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

with AUnit.Test_Cases;

with Orka.Numerics.Doubles.Tensors.CPU;

with Generic_Test_Real_Tensors_Vectors;

package Test_Real_Tensors_CPU_Doubles_Vectors is new Generic_Test_Real_Tensors_Vectors
  ("CPU - Doubles",
   False,
   AUnit.Test_Cases.Test_Case,
   Orka.Float_64,
   Orka.Numerics.Doubles.Tensors,
   Orka.Numerics.Doubles.Tensors.CPU.Real_CPU_Tensor,
   Orka.Numerics.Doubles.Tensors.CPU.Reset_Random);
