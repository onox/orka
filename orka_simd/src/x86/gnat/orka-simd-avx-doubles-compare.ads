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

package Orka.SIMD.AVX.Doubles.Compare is
   pragma Pure;

   type Compare_Mask is new Integer_32 range 0 .. 31;

   function Compare (Left, Right : m256d; Mask : Compare_Mask) return m256d
     with Import, Convention => Intrinsic, External_Name => "__builtin_ia32_cmppd256";

   --  Ordered means a comparison returns False if one of the values is a NaN,
   --  while unordered would return True.
   --
   --  Signaling means an exception is raised for invalid floating-point values.
   --
   --  https://stackoverflow.com/questions/16988199/how-to-choose-avx-compare-predicate-variants

   --  Predicates used are ordered (except negated predicates which are unordered) and signaling

   function "=" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#10#));

   function ">=" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#0D#));

   function ">" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#0E#));

   function "<=" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#02#));

   function "<" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#01#));

   function "/=" (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#14#));
   --  Unordered

   function Not_NaN (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#17#));
   --  True if neither of the elements in Left and Right are NaN, false otherwise

   function NaN (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#13#));
   --  True if either or both elements in Left and Right are NaN, false otherwise

   function Not_Greater_Or_Equal (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#09#));

   function Not_Greater_Than (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#0A#));

   function Not_Less_Equal (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#06#));

   function Not_Less_Than (Left, Right : m256d) return m256d is
     (Compare (Left, Right, 16#05#));

   function Is_True (Elements : m256d; Position : Index_4D) return Boolean;
   --  Return true if an element at the given position is not zero, false otherwise.
   --
   --  A comparison using one of the operators above may result in elements
   --  consisting of all 1's. Trying to directly read such an element by
   --  using an index (like 'Elements (X)' for example) may result
   --  in a Constraint_Error. Use this function to check if an element is
   --  not zero after comparison using one of the operators above.

   function Is_Equal (Left, Right : m256d) return Boolean
     with Inline_Always;

end Orka.SIMD.AVX.Doubles.Compare;
