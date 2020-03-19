--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Ahven; use Ahven;

with GL.Types;

with Orka.SIMD.SSE.Singles;
with Orka.SIMD.FMA.Singles.Arithmetic;

package body Test_SIMD_FMA_Singles_Arithmetic is

   use GL.Types;

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.FMA.Singles.Arithmetic;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Arithmetic");

      T.Add_Test_Routine (Test_Multiply_Vector'Access, "Test '*' operator on matrix and vector");
      T.Add_Test_Routine (Test_Multiply_Matrices'Access, "Test '*' operator on matrices");
   end Initialize;

   procedure Test_Multiply_Vector is
      --  Matrix is an array of columns
      Left  : constant m128_Array := ((1.0, 5.0, 9.0, 13.0),
                                       (2.0, 6.0, 10.0, 14.0),
                                       (3.0, 7.0, 11.0, 15.0),
                                       (4.0, 8.0, 12.0, 16.0));

      Right : constant m128 := (2.0, 1.0, 1.0, 1.0);

      Expected : constant m128 := (11.0, 31.0, 51.0, 71.0);

      Result : constant m128 := Left * Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Multiply_Vector;

   procedure Test_Multiply_Matrices is
      --  Each matrix is an array of columns
      Left  : constant m128_Array := ((1.0, 5.0, 9.0, 13.0),
                                      (2.0, 6.0, 10.0, 14.0),
                                      (3.0, 7.0, 11.0, 15.0),
                                      (4.0, 8.0, 12.0, 16.0));

      Right : constant m128_Array := ((2.0, 1.0, 1.0, 1.0),
                                      (1.0, 2.0, 1.0, 1.0),
                                      (1.0, 1.0, 2.0, 1.0),
                                      (1.0, 1.0, 1.0, 2.0));

      Expected : constant m128_Array := ((11.0, 31.0, 51.0, 71.0),
                                         (12.0, 32.0, 52.0, 72.0),
                                         (13.0, 33.0, 53.0, 73.0),
                                         (14.0, 34.0, 54.0, 74.0));

      Result : constant m128_Array := Left * Right;
   begin
      for I in Index_Homogeneous loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Result (I) (J),
              "Unexpected Double at " & Index_Homogeneous'Image (I));
         end loop;
      end loop;
   end Test_Multiply_Matrices;

end Test_SIMD_FMA_Singles_Arithmetic;
