--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

package body Orka.SIMD.AVX2.Longs.Swizzle is

   function Extract (Elements : m256l; Mask : Unsigned_32) return m128l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_extract128i256";

   function Insert (Left : m256l; Right : m128l; Mask : Unsigned_32) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_insert128i256";

   ----------------------------------------------------------------------------

   function Extract (Elements : m256l; Mask : Lane) return m128l is
     ((case Mask is
          when Lower => Extract (Elements, 0),
          when Upper => Extract (Elements, 1)));

   function Insert (Left : m256l; Right : m128l; Mask : Lane) return m256l is
     ((case Mask is
          when Lower => Insert (Left, Right, 0),
          when Upper => Insert (Left, Right, 1)));

end Orka.SIMD.AVX2.Longs.Swizzle;
