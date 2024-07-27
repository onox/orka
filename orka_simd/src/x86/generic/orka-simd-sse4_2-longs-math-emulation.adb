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

with Orka.SIMD.SSE4_2.Longs.Compare;
with Orka.SIMD.SSE4_1.Longs.Swizzle;

package body Orka.SIMD.SSE4_2.Longs.Math.Emulation is

   use SIMD.SSE4_2.Longs.Compare;
   use SIMD.SSE4_1.Longs.Swizzle;

   function Min (Left, Right : m128l) return m128l is
     (Blend (Left, Right, Right < Left));

   function Max (Left, Right : m128l) return m128l is
     (Blend (Left, Right, Right > Left));

end Orka.SIMD.SSE4_2.Longs.Math.Emulation;
