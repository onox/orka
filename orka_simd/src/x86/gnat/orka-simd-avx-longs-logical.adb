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

package body Orka.SIMD.AVX.Longs.Logical is

   function Test_All_Zero (Elements, Mask : m256l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestz256";

   function Test_All_Ones (Elements, Mask : m256l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestc256";

   function Test_Mix_Ones_Zeros (Elements, Mask : m256l) return Integer
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_ptestnzc256";

   ----------------------------------------------------------------------------

   function Test_All_Zero (Elements, Mask : m256l) return Boolean is
     (Test_All_Zero (Elements, Mask) = 1);

   function Test_All_Ones (Elements, Mask : m256l) return Boolean is
     (Test_All_Ones (Elements, Mask) = 1);

   function Test_Mix_Ones_Zeros (Elements, Mask : m256l) return Boolean is
     (Test_Mix_Ones_Zeros (Elements, Mask) = 1);

end Orka.SIMD.AVX.Longs.Logical;
