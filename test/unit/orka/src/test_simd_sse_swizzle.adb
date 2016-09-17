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

with Orka.SIMD.SSE.Singles.Swizzle;

package body Test_SIMD_SSE_Swizzle is

   use GL.Types;

   use Orka.SIMD;
   use Orka.SIMD.SSE.Singles;
   use Orka.SIMD.SSE.Singles.Swizzle;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Swizzle");

      T.Add_Test_Routine (Test_Shuffle'Access, "Test Shuffle function");
      T.Add_Test_Routine (Test_Transpose_Function'Access, "Test Transpose function");
      T.Add_Test_Routine (Test_Transpose_Procedure'Access, "Test Transpose procedure");
   end Initialize;
 
   procedure Test_Shuffle is
      Elements : constant m128 := (1.0, 2.0, 3.0, 4.0);

      Mask_0_0_0_0 : constant Unsigned_32 := 0 or 0 * 4 or 0 * 16 or 0 * 64;
      Mask_2_2_2_2 : constant Unsigned_32 := 2 or 2 * 4 or 2 * 16 or 2 * 64;
      Mask_1_0_3_2 : constant Unsigned_32 := 1 or 0 * 4 or 3 * 16 or 2 * 64;
      Mask_2_3_0_1 : constant Unsigned_32 := 2 or 3 * 4 or 0 * 16 or 1 * 64;

      Expected : constant array (Positive range <>) of m128
        := ((1.0, 1.0, 1.0, 1.0),
            (3.0, 3.0, 3.0, 3.0),
            (2.0, 1.0, 4.0, 3.0),
            (3.0, 4.0, 1.0, 2.0));

      Results : array (Positive range Expected'Range) of m128;
   begin
      Results (1) := Shuffle (Elements, Elements, Mask_0_0_0_0);
      Results (2) := Shuffle (Elements, Elements, Mask_2_2_2_2);
      Results (3) := Shuffle (Elements, Elements, Mask_1_0_3_2);
      Results (4) := Shuffle (Elements, Elements, Mask_2_3_0_1);

      for I in Expected'Range loop
         for J in Index_Homogeneous loop
            declare
               Message : constant String := "Unexpected Single at " & Index_Homogeneous'Image (J);
            begin
               Assert (Expected (I) (J) = Results (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Shuffle;

   procedure Test_Transpose_Function is
      subtype IH is Index_Homogeneous;

      Elements : constant m128_Array
        := ((1.0,   2.0,  3.0,  4.0),
            (5.0,   6.0,  7.0,  8.0),
            (9.0,  10.0, 11.0, 12.0),
            (13.0, 14.0, 15.0, 16.0));

      Expected : constant m128_Array
        := ((1.0, 5.0,  9.0, 13.0),
            (2.0, 6.0, 10.0, 14.0),
            (3.0, 7.0, 11.0, 15.0),
            (4.0, 8.0, 12.0, 16.0));

      Result : constant m128_Array := Transpose (Elements);
   begin
      for I in Result'Range loop
         for J in Index_Homogeneous loop
            declare
               Message : constant String := "Unexpected Single at " & IH'Image (I) & ", " & IH'Image (J);
            begin
               Assert (Expected (I) (J) = Result (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Transpose_Function;

   procedure Test_Transpose_Procedure is
      subtype IH is Index_Homogeneous;

      Elements : m128_Array
        := ((1.0,   2.0,  3.0,  4.0),
            (5.0,   6.0,  7.0,  8.0),
            (9.0,  10.0, 11.0, 12.0),
            (13.0, 14.0, 15.0, 16.0));

      Expected : constant m128_Array
        := ((1.0, 5.0,  9.0, 13.0),
            (2.0, 6.0, 10.0, 14.0),
            (3.0, 7.0, 11.0, 15.0),
            (4.0, 8.0, 12.0, 16.0));
   begin
      Transpose (Elements);

      for I in Elements'Range loop
         for J in Index_Homogeneous loop
            declare
               Message : constant String := "Unexpected Single at " & IH'Image (I) & ", " & IH'Image (J);
            begin
               Assert (Expected (I) (J) = Elements (I) (J), Message);
            end;
         end loop;
      end loop;
   end Test_Transpose_Procedure;

end Test_SIMD_SSE_Swizzle;
