--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Orka.Transforms.SIMD_Vector_Conversions;
with Orka.Transforms.Doubles.Matrices;
with Orka.Transforms.Singles.Matrices;

with Orka.SIMD.AVX.Doubles.Swizzle;

package Orka.Transforms.Doubles.Vector_Conversions is new Orka.Transforms.SIMD_Vector_Conversions
  (Orka.Transforms.Doubles.Matrices,
   Orka.Transforms.Singles.Matrices,
   SIMD.AVX.Doubles.Swizzle.Convert);
pragma Preelaborate (Orka.Transforms.Doubles.Vector_Conversions);
