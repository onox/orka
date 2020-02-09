--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GL.API;
with GL.Types;

package body GL.Fences is

   overriding procedure Initialize (Object : in out Fence) is
   begin
      Object.Reference := new Sync_Object_Reference'(Sync_ID         => 0,
                                                     Reference_Count => 1,
                                                     Status          => Unset);
   end Initialize;

   overriding procedure Adjust (Object : in out Fence) is
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Fence) is
      use type Low_Level.Sync;

      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Sync_Object_Reference, Name => Sync_Object_Reference_Access);
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count - 1;
         if Object.Reference.Reference_Count = 0 then
            if Object.Reference.Sync_ID /= 0 then
               Fence'Class (Object).Delete;
            end if;
            Free (Object.Reference);
         end if;
      end if;
   end Finalize;

   procedure Set_Fence (Object : in out Fence) is
      GPU_Commands_Complete : constant Low_Level.Enum := 16#9117#;
   begin
      if Object.Initialized then
         Object.Delete;
      end if;
      Object.Reference.Sync_ID := API.Fence_Sync (GPU_Commands_Complete, 0);
      Object.Reference.Status := (if Object.Initialized then Set else Unset);
   end Set_Fence;

   procedure Delete (Object : in out Fence) is
   begin
      API.Delete_Sync (Object.Reference.Sync_ID);
      Object.Reference.Sync_ID := 0;
      Object.Reference.Status := Unset;
   end Delete;

   function Initialized (Object : Fence) return Boolean is
      use type Low_Level.Sync;
   begin
      return Object.Reference.Sync_ID /= 0;
   end Initialized;

   function Status (Object : Fence) return Signaled_Status is
     (Object.Reference.Status);

   function Signaled (Object : Fence) return Boolean is
      use GL.Types;

      Sync_Status : constant := 16#9114#;

      type Signaled_Type is (Unsignaled, Signaled);

      for Signaled_Type use
        (Unsignaled => 16#9118#,
         Signaled   => 16#9119#);
      for Signaled_Type'Size use Low_Level.Enum'Size;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Int, Target => Signaled_Type);
   begin
      if Object.Status = Signaled then
         return True;
      end if;

      declare
         Value : constant Int_Array := API.Get_Sync
           (Object.Reference.Sync_ID, Sync_Status, 1);
      begin
         return Convert (Value (1)) = Signaled;
      end;
   end Signaled;

   function Client_Wait (Object : Fence; Timeout : Duration) return Wait_Status is
      use GL.Types;

      Flush_Commands_Bit  : constant := 16#0000_0001#;
      Timeout_Nanoseconds : constant UInt64 := UInt64 (Timeout * 1e9);

      Result : Wait_Status;
   begin
      Result := API.Client_Wait_Sync
        (Object.Reference.Sync_ID, Flush_Commands_Bit, Timeout_Nanoseconds);

      if Result in Already_Signaled | Condition_Satisfied then
         Object.Reference.Status := Signaled;
      end if;

      return Result;
   end Client_Wait;

   procedure Server_Wait (Object : Fence) is
      Timeout_Ignored : constant := 16#FFF_FFFFF_FFFF_FFFF#;
   begin
      --  Flush the pipeline to ensure that the fence has been sent to the GPU
      GL.Flush;

      API.Wait_Sync (Object.Reference.Sync_ID, 0, Timeout_Ignored);
   end Server_Wait;

   overriding
   function "=" (Left, Right : Fence) return Boolean is
     (Left.Reference = Right.Reference);

end GL.Fences;
