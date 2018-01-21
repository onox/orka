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

private with Orka.Atomics;

limited with Orka.Jobs.Queues;

package Orka.Jobs is
   pragma Preelaborate;

   type Job is limited interface;

   type Job_Ptr is not null access all Job'Class;

   procedure Execute (Object : Job; Queue : Queues.Queue_Ptr) is abstract;
   --  Execute the job, a pointer to the job queue is given so that the job
   --  can schedule new jobs

   function Dependent (Object : Job) return Job_Ptr is abstract;
   --  Return a pointer to the job (if there is any) that depends on this job

   function Decrement_Dependencies (Object : in out Job) return Boolean is abstract;
   --  Decrement the number of dependencies (jobs) that still need to be
   --  executed. Return True if all have been executed, False if there is
   --  at least one job that still needs to run.
   --
   --  When this function returns True, this job can and should be scheduled.

   type Dependency_Array is array (Positive range <>) of Job_Ptr;

   procedure Set_Dependencies
     (Object : access Job; Dependencies : Dependency_Array) is abstract;

   -----------------------------------------------------------------------------

   Null_Job : constant Job_Ptr;

   function Get_Null_Job return Job_Ptr is (Null_Job);
   --  A function that is used when we need a non-static constant (Null_Job)
   --  in a preelaborated unit

   procedure Free (Pointer : in out Job_Ptr)
     with Pre => Pointer /= Null_Job;

   -----------------------------------------------------------------------------

   type Abstract_Job is abstract new Job with private;

   type Parallel_Job is interface and Job;

   type Parallel_Job_Ptr is not null access all Jobs.Parallel_Job'Class;

   procedure Set_Range (Object : in out Parallel_Job; From, To : Positive) is abstract;

   procedure Execute (Object : Parallel_Job; From, To : Positive) is abstract;

   function Parallelize
     (Job : Parallel_Job_Ptr;
      Length, Slice : Positive) return Job_Ptr;

   type Slice_Job is abstract new Abstract_Job and Parallel_Job with private;

   overriding
   procedure Execute (Object : Slice_Job; Queue : Jobs.Queues.Queue_Ptr);

   overriding
   procedure Set_Range (Object : in out Slice_Job; From, To : Positive);

private

   type Empty_Job is new Job with null record;

   overriding
   procedure Execute (Object : Empty_Job; Queue : Queues.Queue_Ptr) is null;

   overriding
   function Dependent (Object : Empty_Job) return Job_Ptr is (Null_Job);

   overriding
   function Decrement_Dependencies (Object : in out Empty_Job) return Boolean is (False);

   overriding
   procedure Set_Dependencies
     (Object : access Empty_Job; Dependencies : Dependency_Array) is null;

   Null_Job : constant Job_Ptr := new Empty_Job;

   -----------------------------------------------------------------------------

   type Abstract_Job is abstract new Job with record
      Dependent    : Job_Ptr := Null_Job;
      Dependencies : Atomics.Unsigned_32 := 0;
   end record;

   overriding
   function Dependent (Object : Abstract_Job) return Job_Ptr is (Object.Dependent);

   overriding
   function Decrement_Dependencies (Object : in out Abstract_Job) return Boolean;

   overriding
   procedure Set_Dependencies
     (Object : access Abstract_Job; Dependencies : Dependency_Array);

   -----------------------------------------------------------------------------

   type Parallel_For_Job is new Abstract_Job with record
      Length, Slice : Positive;
      Job : access Parallel_Job'Class;
   end record;

   overriding
   procedure Execute (Object : Parallel_For_Job; Queue : Jobs.Queues.Queue_Ptr);

   type Slice_Job is abstract new Abstract_Job and Parallel_Job with record
      From, To : Positive;
   end record;

end Orka.Jobs;
