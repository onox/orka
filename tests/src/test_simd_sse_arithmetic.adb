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

with Orka.SIMD.SSE.Singles.Arithmetic;

package body Test_SIMD_SSE_Arithmetic is

   use Orka;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Arithmetic;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - SSE - Arithmetic) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator", Test_Multiply'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '/' operator", Test_Divide'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test exception raised by '/' operator", Test_Divide_By_Zero'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Divide_Or_Zero function", Test_Divide_Or_Zero'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '+' operator", Test_Add'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '-' operator", Test_Subtract'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test unary '-' operator", Test_Minus'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator on matrix and vector", Test_Multiply_Vector'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test '*' operator on matrices", Test_Multiply_Matrices'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test 'abs' operator", Test_Abs'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Sum function", Test_Sum'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Multiply (Object : in out Test) is
      Left  : constant m128 := [1.0, 2.0, 3.0, 4.0];
      Right : constant m128 := [0.0, 1.0, 2.0, 3.0];

      Expected : constant m128 := [0.0, 2.0, 6.0, 12.0];
      Result   : constant m128 := Left * Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Multiply;

   procedure Test_Divide (Object : in out Test) is
      Left  : constant m128 := [0.0, 2.0, 4.0, 4.5];
      Right : constant m128 := [1.0, 1.0, 2.0, 1.5];

      Expected : constant m128 := [0.0, 2.0, 2.0, 3.0];
      Result   : constant m128 := Left / Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Divide;

   procedure Test_Divide_By_Zero (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, 2.0, 3.0];
      Right : constant m128 := [0.0, 0.0, 0.0, 0.0];

      Result : constant m128 := Left / Right;
   begin
      declare
         Result_X : constant Float_32 := Result (X);
         Result_Y : constant Float_32 := Result (Y);
         Result_Z : constant Float_32 := Result (Z);
         Result_W : constant Float_32 := Result (W);
         pragma Unreferenced (Result_X);
         pragma Unreferenced (Result_Y);
         pragma Unreferenced (Result_Z);
         pragma Unreferenced (Result_W);
      begin
         Assert (False, "Expected Constraint_Error exception");
      end;
   exception
      when Constraint_Error =>
         null;
   end Test_Divide_By_Zero;

   procedure Test_Divide_Or_Zero (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, 2.0, 3.0];
      Right : constant m128 := [0.0, 0.0, 0.0, 0.0];

      Expected : constant m128 := [0.0, 0.0, 0.0, 0.0];
      Result   : constant m128 := Divide_Or_Zero (Left, Right);
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Divide_Or_Zero;

   procedure Test_Add (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, 1.5, 2.0];
      Right : constant m128 := [0.0, 0.0, 1.5, 2.0];

      Expected : constant m128 := [0.0, 1.0, 3.0, 4.0];
      Result   : constant m128 := Left + Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Add;

   procedure Test_Subtract (Object : in out Test) is
      Left  : constant m128 := [0.0, 1.0, 1.5, 0.0];
      Right : constant m128 := [0.0, 0.0, 1.5, 2.0];

      Expected : constant m128 := [0.0, 1.0, 0.0, -2.0];
      Result   : constant m128 := Left - Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Subtract;

   procedure Test_Minus (Object : in out Test) is
      Elements : constant m128 := [0.0, 1.0, 1.5, -2.0];

      Expected : constant m128 := [0.0, -1.0, -1.5, 2.0];
      Result   : constant m128 := -Elements;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Minus;

   procedure Test_Multiply_Vector (Object : in out Test) is
      --  Matrix is an array of columns
      Left  : constant m128_Array := [[1.0, 5.0, 9.0, 13.0],
                                      [2.0, 6.0, 10.0, 14.0],
                                      [3.0, 7.0, 11.0, 15.0],
                                      [4.0, 8.0, 12.0, 16.0]];

      Right : constant m128 := [2.0, 1.0, 1.0, 1.0];

      Expected : constant m128 := [11.0, 31.0, 51.0, 71.0];

      Result : constant m128 := Left * Right;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Multiply_Vector;

   procedure Test_Multiply_Matrices (Object : in out Test) is
      --  Each matrix is an array of columns
      Left  : constant m128_Array := [[1.0, 5.0, 9.0, 13.0 ],
                                      [2.0, 6.0, 10.0, 14.0],
                                      [3.0, 7.0, 11.0, 15.0],
                                      [4.0, 8.0, 12.0, 16.0]];

      Right : constant m128_Array := [[2.0, 1.0, 1.0, 1.0],
                                      [1.0, 2.0, 1.0, 1.0],
                                      [1.0, 1.0, 2.0, 1.0],
                                      [1.0, 1.0, 1.0, 2.0]];

      Expected : constant m128_Array := [[11.0, 31.0, 51.0, 71.0],
                                         [12.0, 32.0, 52.0, 72.0],
                                         [13.0, 33.0, 53.0, 73.0],
                                         [14.0, 34.0, 54.0, 74.0]];

      Result : constant m128_Array := Left * Right;
   begin
      for I in Index_4D loop
         for J in Index_4D loop
            Assert (Expected (I) (J) = Result (I) (J), "Unexpected Single at " & I'Image);
         end loop;
      end loop;
   end Test_Multiply_Matrices;

   procedure Test_Abs (Object : in out Test) is
      Elements  : constant m128 := [1.0, -2.0, -3.0, 0.0];

      Expected : constant m128 := [1.0, 2.0, 3.0, 0.0];
      Result   : constant m128 := abs Elements;
   begin
      for I in Index_4D loop
         Assert (Expected (I) = Result (I), "Unexpected Single at " & I'Image);
      end loop;
   end Test_Abs;

   procedure Test_Sum (Object : in out Test) is
      Elements  : constant m128 := [-1.0, 2.0, -3.0, 4.0];

      Expected : constant Float_32 := 2.0;
      Result   : constant Float_32 := Sum (Elements);
   begin
      Assert (Expected = Result, "Unexpected Single " & Result'Image);
   end Test_Sum;

end Test_SIMD_SSE_Arithmetic;
