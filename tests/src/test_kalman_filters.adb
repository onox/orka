--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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
with AUnit.Test_Fixtures;

with Orka.Numerics.Doubles.Tensors.CPU;
with Orka.Numerics.Kalman.CDKF;
with Orka.Numerics.Kalman.UKF;

with Orka.OS;

package body Test_Kalman_Filters is

   use Orka.Numerics.Doubles.Tensors;
   use Orka.Numerics.Doubles.Tensors.CPU;
   use type Orka.Numerics.Doubles.Tensors.Real_Element;

   package Random is new Generic_Random (Real_CPU_Tensor);
   package Kalman is new Orka.Numerics.Kalman (Orka.Numerics.Doubles.Tensors, Real_CPU_Tensor);

   package Kalman_CDKF is new Kalman.CDKF;
   package Kalman_UKF  is new Kalman.UKF;

   use AUnit.Assertions;

   ----------------------------------------------------------------------------

   function Get_Measurements (Count : Positive; Std_Dev : Element) return Real_CPU_Tensor is
      Shape : constant Orka.Numerics.Tensor_Shape := [1, Count];
      Stop  : constant Element := Element (Count);
   begin
      Reset_Random (Orka.OS.Monotonic_Clock);

      declare
         Indices : constant Real_CPU_Tensor := Linear_Space (1.0, Stop, Count => Count).Reshape (Shape);
         X, Y    : constant Real_CPU_Tensor := Indices + Random.Normal (Shape) * Std_Dev;
      begin
         return Real_CPU_Tensor'(X & Y).Transpose;
      end;
   end Get_Measurements;

   --  The matrices below are from Section 10.7 of [1]
   --
   --  [1] "Kalman and Bayesian Filters in Python", Labbe Jr R., 2020,
   --      https://github.com/rlabbe/Kalman-and-Bayesian-Filters-in-Python

   Std_Dev  : constant := 0.3;

   Max_Std_Dev : constant := 0.4;
   Max_Mean    : constant := 0.1;

   Actual   : constant Real_CPU_Tensor := Get_Measurements (100, 0.0);
   Elements : constant Real_CPU_Tensor := Get_Measurements (100, Std_Dev);

   Q : constant Kalman.Matrix := To_Tensor
     ([0.005, 0.01, 0.0, 0.0,
       0.01, 0.02, 0.0, 0.0,
       0.0, 0.0, 0.005, 0.01,
       0.0, 0.0, 0.01, 0.02],
      Shape => [4, 4]);

   R : constant Kalman.Matrix := Diagonal ([Std_Dev**2, Std_Dev**2]);

   DT  : constant Duration := 1.0;
   DTE : constant Element  := Element (DT);

   F : constant Kalman.Matrix := To_Tensor
     ([1.0, DTE, 0.0, 0.0,
       0.0, 1.0, 0.0, 0.0,
       0.0, 0.0, 1.0, DTE,
       0.0, 0.0, 0.0, 1.0],
      Shape => [4, 4]);

   function F_Cv (Point : Kalman.Vector; DT : Orka.Float_64) return Kalman.Vector is (F * Point);

   function H_Cv (Point : Kalman.Vector) return Kalman.Vector is
     (To_Tensor ([Element'(Point (1)), Element'(Point (3))]));

   ----------------------------------------------------------------------------

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_UKF_Kalman_Filter (Object : in out Test) is
      Filter : Kalman.Filter := Kalman_UKF.Create_Filter
        (Q       => Q,
         R       => R,
         Weights => Kalman_UKF.Weights (N => 4, A => 0.1, B => 2.0, K => 1.0));

      Filtered_Elements : Real_CPU_Tensor := Empty (Elements.Shape);
   begin
      for I in 1 .. Elements.Rows loop
         Kalman_UKF.Predict_Update (Filter, F_Cv'Access, H_Cv'Access, DT, Elements (I));
         Filtered_Elements.Set (I, H_Cv (Filter.State));
      end loop;

      declare
         Filter_Std_Dev : constant Element := Standard_Deviation (Filtered_Elements -  Actual);
         Filter_Mean    : constant Element := Mean (Filtered_Elements -  Actual);
      begin
         Assert (Filter_Std_Dev <= Max_Std_Dev,
           "Standard deviation of filtered measurements (" & Filter_Std_Dev'Image & ")" &
           " higher than " & Max_Std_Dev'Image);
         Assert (Filter_Mean <= Max_Mean,
           "Mean of filtered measurements (" & Filter_Mean'Image & ")" &
           " higher than " & Max_Mean'Image);
      end;
   end Test_UKF_Kalman_Filter;

   procedure Test_SR_CDKF_Kalman_Filter (Object : in out Test) is
      Filter : Kalman.Filter := Kalman_CDKF.Create_Filter
        (Q       => Q + 16.0 * Identity (4),
         R       => R,
         Weights => Kalman_CDKF.Weights (N => 4, H2 => 3.0));

      Filtered_Elements : Real_CPU_Tensor := Empty (Elements.Shape);
   begin
      for I in 1 .. Elements.Rows loop
         Kalman_CDKF.Predict_Update (Filter, F_Cv'Access, H_Cv'Access, DT, Elements (I));
         Filtered_Elements.Set (I, H_Cv (Filter.State));
      end loop;

      declare
         Filter_Std_Dev : constant Element := Standard_Deviation (Filtered_Elements -  Actual);
         Filter_Mean    : constant Element := Mean (Filtered_Elements -  Actual);
      begin
         Assert (Filter_Std_Dev <= Max_Std_Dev,
           "Standard deviation of filtered measurements (" & Filter_Std_Dev'Image & ")" &
           " higher than " & Max_Std_Dev'Image);
         Assert (Filter_Mean <= Max_Mean,
           "Mean of filtered measurements (" & Filter_Mean'Image & ")" &
           " higher than " & Max_Mean'Image);
      end;
   end Test_SR_CDKF_Kalman_Filter;

   ----------------------------------------------------------------------------

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Filters - Sigma-point Kalman) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test updating UKF filter", Test_UKF_Kalman_Filter'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test updating SR-CDKF filter", Test_SR_CDKF_Kalman_Filter'Access));

      return Test_Suite'Access;
   end Suite;
end Test_Kalman_Filters;
