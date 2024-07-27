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

package body Orka.SIMD.SSE2.Integers.Swizzle is

   Mask_2_2_4_4 : constant := 1 + 1 * 4 + 3 * 16 + 3 * 64;
   Mask_1_1_3_3 : constant := 0 + 0 * 4 + 2 * 16 + 2 * 64;

   function Duplicate_H (Elements : m128i) return m128i is
     (Permute (Elements, Mask_2_2_4_4));

   function Duplicate_L (Elements : m128i) return m128i is
     (Permute (Elements, Mask_1_1_3_3));

end Orka.SIMD.SSE2.Integers.Swizzle;
