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

with Orka.SIMD.AVX.Doubles.Swizzle;

package body Test_SIMD_AVX_Swizzle is

   use GL.Types;

   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Swizzle;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Swizzle");

      T.Add_Test_Routine (Test_Shuffle'Access, "Test Shuffle function");
      T.Add_Test_Routine (Test_Shuffle_Across_Lanes'Access, "Test shuffling across lanes");
      T.Add_Test_Routine (Test_Permute_Lanes'Access, "Test Permute_Lanes function");
      T.Add_Test_Routine (Test_Blend'Access, "Test Blend function");
      T.Add_Test_Routine (Test_Transpose_Function'Access, "Test Transpose function");
      T.Add_Test_Routine (Test_Transpose_Procedure'Access, "Test Transpose procedure");
   end Initialize;

   procedure Test_Shuffle is
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
            declare
               Message : constant String := "Unexpected Double at " & Index_Homogeneous'Image (J);
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Shuffle;

   procedure Test_Shuffle_Across_Lanes is
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
            declare
               Message : constant String := "Unexpected Double at " & Index_Homogeneous'Image (J);
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Shuffle_Across_Lanes;

   procedure Test_Permute_Lanes is
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
               Message : constant String := "Unexpected Double at " & Index_Homogeneous'Image (J);
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Permute_Lanes;

   procedure Test_Blend is
      Left  : constant m256d := (1.0, 2.0, 3.0, 4.0);
      Right : constant m256d := (5.0, 6.0, 7.0, 8.0);

      Expected : constant m256d := (1.0, 6.0, 7.0, 4.0);
      Result   : constant m256d := Blend (Left, Right, Mask => 2#0110#);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Blend;

   procedure Test_Transpose_Function is
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
            declare
               Message : constant String := "Unexpected Double at " & IH'Image (I) & ", " & IH'Image (J);
            begin
               Assert (Expected (I) (J) = Result (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Transpose_Function;

   procedure Test_Transpose_Procedure is
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
            declare
               Message : constant String := "Unexpected Double at " & IH'Image (I) & ", " & IH'Image (J);
            begin
               Assert (Expected (I) (J) = Elements (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Transpose_Procedure;

end Test_SIMD_AVX_Swizzle;
