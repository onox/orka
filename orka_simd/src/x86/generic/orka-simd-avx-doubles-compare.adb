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

with Ada.Unchecked_Conversion;

with Orka.SIMD.AVX.Doubles.Arithmetic;
with Orka.SIMD.AVX.Longs.Logical;

package body Orka.SIMD.AVX.Doubles.Compare is

   use SIMD.AVX.Doubles.Arithmetic;
   use SIMD.AVX.Longs.Logical;
   use SIMD.AVX.Longs;

   function Is_True (Elements : m256d; Position : Index_4D) return Boolean is
   begin
      return Elements (Position) /= 0.0;
   exception
      when Constraint_Error =>
         return True;
   end Is_True;

   function Is_Equal (Left, Right : m256d) return Boolean is
      Epsilon  : constant := Float_64'Model_Epsilon;

      function Convert is new Ada.Unchecked_Conversion (m256d, m256l);

      Result : constant m256d :=
        abs (Left - Right) <= (Epsilon, Epsilon, Epsilon, Epsilon);
   begin
      return Test_All_Ones (Convert (Result), Convert (Result = Result));
   end Is_Equal;

end Orka.SIMD.AVX.Doubles.Compare;
