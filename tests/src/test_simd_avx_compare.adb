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

with AUnit.Assertions;
with AUnit.Test_Caller;

with Orka.SIMD.AVX.Doubles.Arithmetic;
with Orka.SIMD.AVX.Doubles.Compare;

package body Test_SIMD_AVX_Compare is

   use Orka;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Compare;

   type Is_True_Array is array (Index_4D) of Boolean;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - AVX - Compare) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '=' operator", Test_Equal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/=' operator", Test_Not_Equal'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '>' operator", Test_Greater_Than'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '<' operator", Test_Less_Than'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test NaN function", Test_Nan'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Not_NaN function", Test_Not_Nan'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Equal (Object : in out Test) is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (False, False, True, False);
      Result   : constant m256d := Left = Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Equal;

   procedure Test_Not_Equal (Object : in out Test) is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (True, True, False, True);
      Result   : constant m256d := Left /= Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Not_Equal;

   procedure Test_Greater_Than (Object : in out Test) is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (False, True, False, False);
      Result   : constant m256d := Left > Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Greater_Than;

   procedure Test_Less_Than (Object : in out Test) is
      Left  : constant m256d := (-0.2, 1.0, 1.5, 0.0);
      Right : constant m256d := (0.0, 0.0, 1.5, 2.0);

      Expected : constant Is_True_Array := (True, False, False, True);
      Result   : constant m256d := Left < Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Less_Than;

   procedure Test_Nan (Object : in out Test) is
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
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Nan;

   procedure Test_Not_Nan (Object : in out Test) is
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
      for I in Index_4D loop
         Assert (Expected (I) = Is_True (Result, I), "Unexpected result at " & I'Image);
      end loop;
   end Test_Not_Nan;

end Test_SIMD_AVX_Compare;
