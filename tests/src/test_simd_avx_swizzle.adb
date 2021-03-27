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

with Orka.SIMD.AVX.Doubles.Swizzle;

package body Test_SIMD_AVX_Swizzle is

   use Orka;
   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Swizzle;

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(SIMD - AVX - Swizzle) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Shuffle function", Test_Shuffle'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test shuffling across lanes", Test_Shuffle_Across_Lanes'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Permute_Lanes function", Test_Permute_Lanes'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Blend function", Test_Blend'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Transpose function", Test_Transpose_Function'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test Transpose procedure", Test_Transpose_Procedure'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Shuffle (Object : in out Test) is
      Elements : constant m256d := (1.0, 2.0, 3.0, 4.0);

      Mask_0_0_0_0 : constant Unsigned_32 := 0 or 0 * 2 or 0 * 4 or 0 * 8;
      Mask_1_1_1_1 : constant Unsigned_32 := 1 or 1 * 2 or 1 * 4 or 1 * 8;
      Mask_1_0_1_1 : constant Unsigned_32 := 1 or 0 * 2 or 1 * 4 or 1 * 8;
      Mask_1_1_0_1 : constant Unsigned_32 := 1 or 1 * 2 or 0 * 4 or 1 * 8;

      Expected : constant array (Positive range <>) of m256d
        := ((1.0, 1.0, 3.0, 3.0),
            (2.0, 2.0, 4.0, 4.0),
            (2.0, 1.0, 4.0, 4.0),
            (2.0, 2.0, 3.0, 4.0));

      Results : array (Positive range Expected'Range) of m256d;
   begin
      Results (1) := Shuffle_Within_Lanes (Elements, Elements, Mask_0_0_0_0);
      Results (2) := Shuffle_Within_Lanes (Elements, Elements, Mask_1_1_1_1);
      Results (3) := Shuffle_Within_Lanes (Elements, Elements, Mask_1_0_1_1);
      Results (4) := Shuffle_Within_Lanes (Elements, Elements, Mask_1_1_0_1);

      for I in Expected'Range loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Results (I) (J), "Unexpected Double at " & J'Image);
         end loop;
      end loop;
   end Test_Shuffle;

   procedure Test_Shuffle_Across_Lanes (Object : in out Test) is
      Elements : constant m256d := (1.0, 2.0, 3.0, 4.0);

      Mask_0_0_0_0 : constant Unsigned_32 := 0 or 0 * 2 or 0 * 4 or 0 * 8;
      Mask_1_1_0_0 : constant Unsigned_32 := 1 or 1 * 2 or 0 * 4 or 0 * 8;
      Mask_0_0_1_1 : constant Unsigned_32 := 0 or 0 * 2 or 1 * 4 or 1 * 8;

      Expected : constant array (Positive range <>) of m256d
        := ((1.0, 1.0, 1.0, 1.0),
            (2.0, 2.0, 2.0, 2.0),
            (3.0, 3.0, 3.0, 3.0),
            (4.0, 4.0, 4.0, 4.0));

      Results : array (Positive range Expected'Range) of m256d;
   begin
      Results (1) := Shuffle_Within_Lanes (Elements, Elements, Mask_0_0_0_0);
      Results (2) := Shuffle_Within_Lanes (Elements, Elements, Mask_1_1_0_0);
      Results (3) := Shuffle_Within_Lanes (Elements, Elements, Mask_0_0_0_0);
      Results (4) := Shuffle_Within_Lanes (Elements, Elements, Mask_0_0_1_1);

      Results (1) := Duplicate_LH (Results (1));
      Results (2) := Duplicate_LH (Results (2));
      Results (3) := Duplicate_HL (Results (3));
      Results (4) := Duplicate_HL (Results (4));

      for I in Expected'Range loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Results (I) (J), "Unexpected Double at " & J'Image);
         end loop;
      end loop;
   end Test_Shuffle_Across_Lanes;

   procedure Test_Permute_Lanes (Object : in out Test) is
      Elements : constant m256d := (1.0, 2.0, 3.0, 4.0);

      Mask_1_0_0_0 : constant Unsigned_32 := 1 or 0 * 16 or 0 * 8 or 0 * 128;
      Mask_0_1_0_0 : constant Unsigned_32 := 0 or 1 * 16 or 0 * 8 or 0 * 128;
      Mask_2_1_0_1 : constant Unsigned_32 := 2 or 1 * 16 or 0 * 8 or 1 * 128;
      Mask_2_3_1_0 : constant Unsigned_32 := 2 or 3 * 16 or 1 * 8 or 0 * 128;

      Expected : constant array (Positive range <>) of m256d
        := ((3.0, 4.0, 1.0, 2.0),
            (1.0, 2.0, 3.0, 4.0),
            (1.0, 2.0, 0.0, 0.0),
            (0.0, 0.0, 3.0, 4.0));

      Results : array (Positive range Expected'Range) of m256d;
   begin
      Results (1) := Permute_Lanes (Elements, Elements, Mask_1_0_0_0);
      Results (2) := Permute_Lanes (Elements, Elements, Mask_0_1_0_0);
      Results (3) := Permute_Lanes (Elements, Elements, Mask_2_1_0_1);
      Results (4) := Permute_Lanes (Elements, Elements, Mask_2_3_1_0);

      for I in Expected'Range loop
         for J in Index_Homogeneous loop
            declare
               Message : constant String := "Unexpected Double at " & J'Image;
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Permute_Lanes;

   procedure Test_Blend (Object : in out Test) is
      Left  : constant m256d := (1.0, 2.0, 3.0, 4.0);
      Right : constant m256d := (5.0, 6.0, 7.0, 8.0);

      Expected : constant m256d := (1.0, 6.0, 7.0, 4.0);
      Result   : constant m256d := Blend (Left, Right, Mask => 2#0110#);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & I'Image);
      end loop;
   end Test_Blend;

   procedure Test_Transpose_Function (Object : in out Test) is
      subtype IH is Index_Homogeneous;

      Elements : constant m256d_Array
        := ((1.0,   2.0,  3.0,  4.0),
            (5.0,   6.0,  7.0,  8.0),
            (9.0,  10.0, 11.0, 12.0),
            (13.0, 14.0, 15.0, 16.0));

      Expected : constant m256d_Array
        := ((1.0, 5.0,  9.0, 13.0),
            (2.0, 6.0, 10.0, 14.0),
            (3.0, 7.0, 11.0, 15.0),
            (4.0, 8.0, 12.0, 16.0));

      Result : constant m256d_Array := Transpose (Elements);
   begin
      for I in Result'Range loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Result (I) (J),
              "Unexpected Double at " & I'Image & ", " & J'Image);
         end loop;
      end loop;
   end Test_Transpose_Function;

   procedure Test_Transpose_Procedure (Object : in out Test) is
      subtype IH is Index_Homogeneous;

      Elements : m256d_Array
        := ((1.0,   2.0,  3.0,  4.0),
            (5.0,   6.0,  7.0,  8.0),
            (9.0,  10.0, 11.0, 12.0),
            (13.0, 14.0, 15.0, 16.0));

      Expected : constant m256d_Array
        := ((1.0, 5.0,  9.0, 13.0),
            (2.0, 6.0, 10.0, 14.0),
            (3.0, 7.0, 11.0, 15.0),
            (4.0, 8.0, 12.0, 16.0));
   begin
      Transpose (Elements);

      for I in Elements'Range loop
         for J in Index_Homogeneous loop
            Assert (Expected (I) (J) = Elements (I) (J),
              "Unexpected Double at " & I'Image & ", " & J'Image);
         end loop;
      end loop;
   end Test_Transpose_Procedure;

end Test_SIMD_AVX_Swizzle;
