--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package Orka.SIMD.AVX.Doubles is
   pragma Pure;

   type m256d is array (Index_4D) of Float_64
     with Alignment => 32;
   pragma Machine_Attribute (m256d, "vector_type");

   type m256d_Array is array (Index_4D) of m256d
     with Alignment => 32;

end Orka.SIMD.AVX.Doubles;
