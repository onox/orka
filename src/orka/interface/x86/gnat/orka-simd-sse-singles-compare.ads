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

package Orka.SIMD.SSE.Singles.Compare is
   pragma Preelaborate;

   function "=" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpeqps";

   function ">=" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpgeps";

   function ">" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpgtps";

   function "<=" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpleps";

   function "<" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpltps";

   function "/=" (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpneqps";

   function Not_Greater_Or_Equal (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpngeps";

   function Not_Greater_Than (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpngtps";

   function Not_Less_Equal (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpnleps";

   function Not_Less_Than (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpnltps";

   function Not_Nan (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpordps";

   function Nan (Left, Right : m128) return m128
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmpunordps";

end Orka.SIMD.SSE.Singles.Compare;
