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

with Ada.Finalization;

with GL.Low_Level;

package GL.Fences is
   pragma Preelaborate;

   type Wait_Status is
     (Already_Signaled,
      Timeout_Expired,
      Condition_Satisfied,
      Wait_Failed);

   type Signaled_Status is (Unset, Set, Signaled);

   type Fence is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Object : in out Fence);
   --  Create the object in OpenGL memory

   overriding procedure Adjust (Object : in out Fence);
   --  Increase the reference count

   overriding procedure Finalize (Object : in out Fence);
   --  Decrease the reference count, deleting the sync object when it
   --  reaches zero

   procedure Set_Fence (Object : in out Fence)
     with Pre  => Object.Status /= Set,
          Post => Object.Initialized and Object.Status = Set;
   --  Insert a new fence sync object into the OpenGL command stream
   --
   --  Any previously associated fence will be deleted

   procedure Delete (Object : in out Fence)
     with Post => not Object.Initialized and Object.Status = Unset;

   function Initialized (Object : Fence) return Boolean;
   --  Return True if a fence sync object is associated, False otherwise

   function Status (Object : Fence) return Signaled_Status;

   function Signaled (Object : Fence) return Boolean
     with Pre => Object.Initialized and Object.Status /= Unset;

   function Client_Wait (Object : Fence; Timeout : Duration) return Wait_Status
     with Pre => Object.Initialized and Object.Status = Set;
   --  Let the client block and wait until the fence becomes signaled or the
   --  timeout expires
   --
   --  Does not delete the fence if it has been signaled.

   procedure Server_Wait (Object : Fence)
     with Pre => Object.Initialized and Object.Status = Set;
   --  Let the server block and wait until the fence becomes signaled or the
   --  video driver-dependent maximum timeout expires
   --
   --  Does not delete the fence. Call Signaled to find out
   --  whether the fence was signaled or the timeout expired.

   overriding
   function "=" (Left, Right : Fence) return Boolean;

private

   type Sync_Object_Reference is record
      Sync_ID         : Low_Level.Sync;
      Reference_Count : Natural;
      Status          : Signaled_Status;
   end record;

   type Sync_Object_Reference_Access is access all Sync_Object_Reference;

   type Fence is new Ada.Finalization.Controlled with record
      Reference : Sync_Object_Reference_Access;
   end record;

   for Wait_Status use
     (Already_Signaled    => 16#911A#,
      Timeout_Expired     => 16#911B#,
      Condition_Satisfied => 16#911C#,
      Wait_Failed         => 16#911D#);
   for Wait_Status'Size use Low_Level.Enum'Size;

end GL.Fences;
