--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with GL.API;
with GL.Enums.Getter;
with GL.Low_Level;

package body GL.Compute is

   Indices : constant array (Index_3D) of UInt := (X => 0, Y => 1, Z => 2);

   procedure Dispatch_Compute (X, Y, Z : UInt := 1) is
   begin
      API.Dispatch_Compute (X, Y, Z);
   end Dispatch_Compute;

   procedure Dispatch_Compute_Indirect (Offset : Size) is
   begin
      API.Dispatch_Compute_Indirect (Low_Level.IntPtr (Offset));
   end Dispatch_Compute_Indirect;

   function Max_Compute_Shared_Memory_Size return Size is
      Value : Size := 0;
   begin
      API.Get_Size (Enums.Getter.Max_Compute_Shared_Memory_Size, Value);
      return Value;
   end Max_Compute_Shared_Memory_Size;

   function Max_Compute_Work_Group_Invocations return Size is
      Value : Size := 0;
   begin
      API.Get_Size (Enums.Getter.Max_Compute_Work_Group_Invocations, Value);
      return Value;
   end Max_Compute_Work_Group_Invocations;

   function Max_Compute_Work_Group_Count return Dimension_Size_Array is
      Values : Dimension_Size_Array := (others => 0);
   begin
      for Dimension in Values'Range loop
         API.Get_Size_I (Enums.Getter.Max_Compute_Work_Group_Count,
           Indices (Dimension), Values (Dimension));
      end loop;
      return Values;
   end Max_Compute_Work_Group_Count;

   function Max_Compute_Work_Group_Size return Dimension_Size_Array is
      Values : Dimension_Size_Array := (others => 0);
   begin
      for Dimension in Values'Range loop
         API.Get_Size_I (Enums.Getter.Max_Compute_Work_Group_Size,
           Indices (Dimension), Values (Dimension));
      end loop;
      return Values;
   end Max_Compute_Work_Group_Size;

end GL.Compute;
