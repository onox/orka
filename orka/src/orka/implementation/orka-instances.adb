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

with Orka.Logging;

package body Orka.Instances is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;

   procedure Log is new Orka.Logging.Generic_Log (Third_Party);

   function Create_Manager (Capacity, Parts : Positive) return Manager is
      Transforms : constant Positive := Capacity * Parts;

      Data : Instances_Type (Capacity);
   begin
      return Result : Manager do
         Result.Added    := 0;
         Result.Capacity := Capacity;
         Result.Parts    := Parts;

         for Index in Data.Instances'Range loop
            Data.Instances (Index) := Index;
            Data.Indices   (Index) := Index;
         end loop;

         Result.Instances := Instances_Holder.To_Holder (Data);

         --  Set-up a mapped buffer for world transform matrices
         Result.Transforms := PMB.Create_Buffer
           (Orka.Types.Single_Matrix_Type, Transforms, Rendering.Buffers.Mapped.Write);

         Log (Debug, "Created group for" &
           Capacity'Image & " instances");
         Log (Debug, " " &
           Capacity'Image & " instances x" & Parts'Image & " parts =" &
           Transforms'Image & " transforms");
      end return;
   end Create_Manager;

   function Add_Instance (Object : in out Manager) return Cursor is
      Instance_Index : constant Index := Object.Added + 1;
      Instance_ID : Instance;

      procedure Get_Instance (Data : Instances_Type) is
      begin
         Instance_ID := Data.Instances (Instance_Index);
         pragma Assert (Data.Indices (Instance_ID) = Instance_Index);
      end Get_Instance;
   begin
      if Object.Length = Object.Capacity then
         raise Constraint_Error with "Transforms buffer already has maximum number of instances";
      end if;

      Object.Instances.Query_Element (Get_Instance'Access);
      Object.Added := Object.Added + 1;

      return Cursor (Instance_ID);
   end Add_Instance;

   procedure Remove_Instance (Object : in out Manager; Instance : Cursor) is
      Left_Instance : constant Instances.Instance := Instances.Instance (Instance);

      Left_Index  : constant Index := Object.Instances.Element.Indices (Left_Instance);
      Right_Index : constant Index := Object.Added;
      pragma Assert (Object.Instances.Element.Instances (Left_Index) = Left_Instance);

      procedure Swap_Instances (Data : in out Instances_Type) is
         Right_Instance : constant Instances.Instance := Data.Instances (Right_Index);
      begin
         Data.Instances (Left_Index)  := Right_Instance;
         Data.Instances (Right_Index) := Left_Instance;

         Data.Indices (Left_Instance)  := Right_Index;
         Data.Indices (Right_Instance) := Left_Index;
      end Swap_Instances;
   begin
      if Left_Index > Right_Index then
         raise Constraint_Error with "Instance index out of range";
      end if;

      Object.Instances.Update_Element (Swap_Instances'Access);

      Object.Added := Object.Added - 1;
   end Remove_Instance;

   procedure Set_Transform
     (Object   : in out Manager;
      Value    : Orka.Types.Singles.Matrix4;
      Instance : Cursor;
      Part     : Natural)
   is
      Instance_Index : Index;

      procedure Get_Instance (Data : Instances_Type) is
      begin
         Instance_Index := Data.Indices (Instances.Instance (Instance));
      end Get_Instance;
   begin
      Object.Instances.Query_Element (Get_Instance'Access);

      if Instance_Index > Object.Added then
         raise Constraint_Error with "Instance index out of range";
      end if;

      if Part >= Object.Parts then
         raise Constraint_Error with "Part index out of range";
      end if;

      Object.Transforms.Write_Data (Value, (Instance_Index - Index'First) + Part * Object.Added);
   end Set_Transform;

   function Transforms (Object : Manager) return Rendering.Buffers.Bindable_Buffer'Class is
     (Object.Transforms);

   function Parts (Object : Manager) return Positive is (Object.Parts);

   function Length (Object : Manager) return Natural is (Object.Added);

   function Capacity (Object : Manager) return Positive is (Object.Capacity);

   procedure Complete_Frame (Object : in out Manager) is
   begin
      Object.Transforms.Advance_Index;
   end Complete_Frame;

end Orka.Instances;
