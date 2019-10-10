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

with Orka.SIMD.AVX.Doubles.Math;

package body Test_SIMD_AVX_Math is

   use GL.Types;
   use Orka.SIMD;
   use Orka.SIMD.AVX.Doubles;
   use Orka.SIMD.AVX.Doubles.Math;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Math");

      T.Add_Test_Routine (Test_Max'Access, "Test Max function");
      T.Add_Test_Routine (Test_Min'Access, "Test Min function");
      T.Add_Test_Routine (Test_Nearest_Integer'Access, "Test Round_Nearest_Integer function");
      T.Add_Test_Routine (Test_Floor'Access, "Test Floor function");
      T.Add_Test_Routine (Test_Ceil'Access, "Test Ceil function");
      T.Add_Test_Routine (Test_Truncate'Access, "Test Round_Truncate function");
   end Initialize;

   procedure Test_Max is
      Left  : constant m256d := (0.0, 1.0, -2.0, 1.0);
      Right : constant m256d := (0.0, 0.0, 1.0, 2.0);

      Expected : constant m256d := (0.0, 1.0, 1.0, 2.0);
      Result   : constant m256d := Max (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Max;

   procedure Test_Min is
      Left  : constant m256d := (0.0, 1.0, -2.0, 1.0);
      Right : constant m256d := (0.0, 0.0, 1.0, 2.0);

      Expected : constant m256d := (0.0, 0.0, -2.0, 1.0);
      Result   : constant m256d := Min (Left, Right);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Min;

   procedure Test_Nearest_Integer is
      Elements : constant m256d := (-1.5, 0.6, -0.4, 1.9);
      Expected : constant m256d := (-2.0, 1.0,  0.0, 2.0);

      Result : constant m256d := Round_Nearest_Integer (Elements);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Nearest_Integer;

   procedure Test_Floor is
      Elements : constant m256d := (-1.5, 0.6, -0.4, 1.9);
      Expected : constant m256d := (-2.0, 0.0, -1.0, 1.0);

      Result : constant m256d := Floor (Elements);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Floor;

   procedure Test_Ceil is
      Elements : constant m256d := (-1.5, 0.2, -0.4, 1.9);
      Expected : constant m256d := (-1.0, 1.0,  0.0, 2.0);

      Result : constant m256d := Ceil (Elements);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Ceil;

   procedure Test_Truncate is
      Elements : constant m256d := (-1.5, 0.2, -0.4, 1.9);
      Expected : constant m256d := (-1.0, 0.0,  0.0, 1.0);

      Result : constant m256d := Round_Truncate (Elements);
   begin
      for I in Index_Homogeneous loop
         Assert (Expected (I) = Result (I), "Unexpected Double at " & Index_Homogeneous'Image (I));
      end loop;
   end Test_Truncate;

end Test_SIMD_AVX_Math;
