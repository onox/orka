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

with Ahven; use Ahven;

with GL.Types;

with Orka.SIMD.AVX.Doubles.Arithmetic;
with Orka.SIMD.AVX.Doubles.Compare;

package body Test_SIMD_AVX_Compare is

   use GL.Types;

   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Compare;

   type Is_True_Array is array (Index_Homogeneous) of Boolean;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Compare");

      T.Add_Test_Routine (Test_Equal'Access, "Test '=' operator");
      T.Add_Test_Routine (Test_Not_Equal'Access, "Test '/=' operator");
      T.Add_Test_Routine (Test_Greater_Than'Access, "Test '>' operator");
      T.Add_Test_Routine (Test_Less_Than'Access, "Test '<' operator");
      T.Add_Test_Routine (Test_Nan'Access, "Test NaN function");
      T.Add_Test_Routine (Test_Not_Nan'Access, "Test Not_NaN function");
   end Initialize;

   procedure Test_Equal is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (False, False, True, False);
      Result   : constant m256d := Left = Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Equal;

   procedure Test_Not_Equal is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (True, True, False, True);
      Result   : constant m256d := Left /= Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Not_Equal;

   procedure Test_Greater_Than is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (False, True, False, False);
      Result   : constant m256d := Left > Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Greater_Than;

   procedure Test_Less_Than is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (True, False, False, True);
      Result   : constant m256d := Left < Right;
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Less_Than;

   procedure Test_Nan is
      use Orka.SIMD.AVX.Doubles.Arithmetic;

      --  Nan, 0.0, 1.0, Nan
      A  : constant m256d := (0.0, 0.0, 1.0, 0.0);
      B  : constant m256d := (0.0, 1.0, 1.0, 0.0);

      --  0.0, 1.0, Nan, Nan
      C  : constant m256d := (0.0, 1.0, 0.0, 0.0);
      D  : constant m256d := (1.0, 1.0, 0.0, 0.0);

      Left  : constant m256d := A / B;
      Right : constant m256d := C / D;

      Expected : constant Is_True_Array := (True, False, True, True);
      Result   : constant m256d := Nan (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Nan;

   procedure Test_Not_Nan is
      use Orka.SIMD.AVX.Doubles.Arithmetic;

      --  Nan, 0.0, 1.0, Nan
      A  : constant m256d := (0.0, 0.0, 1.0, 0.0);
      B  : constant m256d := (0.0, 1.0, 1.0, 0.0);

      --  0.0, 1.0, Nan, Nan
      C  : constant m256d := (0.0, 1.0, 0.0, 0.0);
      D  : constant m256d := (1.0, 1.0, 0.0, 0.0);

      Left  : constant m256d := A / B;
      Right : constant m256d := C / D;

      Expected : constant Is_True_Array := (False, True, False, False);
      Result   : constant m256d := Not_Nan (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Not_Nan;

end Test_SIMD_AVX_Compare;
