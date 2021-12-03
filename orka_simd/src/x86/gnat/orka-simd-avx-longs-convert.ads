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

with Orka.SIMD.AVX.Doubles;

package Orka.SIMD.AVX.Longs.Convert is
   pragma Pure;

   use SIMD.AVX.Doubles;

   function Convert (Elements : m256d) return m256l
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cvtpd2dq256";

   function Convert (Elements : m256l) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cvtdq2pd256";

end Orka.SIMD.AVX.Longs.Convert;
