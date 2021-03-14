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

private with Ada.Finalization;

private with Orka.Atomics;

package Orka.Jobs is
   pragma Preelaborate;

   type Job is limited interface;

   type Job_Ptr is not null access all Job'Class;

   type Execution_Context is limited interface;

   procedure Enqueue (Object : Execution_Context; Element : Job_Ptr) is abstract;

   procedure Execute
     (Object  : Job;
      Context : Execution_Context'Class) is abstract;
   --  Execute the job. The job can insert extra jobs between itself and its
   --  Dependent by calling the Enqueue procedure

   function Dependent (Object : Job) return Job_Ptr is abstract;
   --  Return a pointer to the job (if there is any) that depends on this job

   function Decrement_Dependencies (Object : in out Job) return Boolean is abstract;
   --  Decrement the number of dependencies (jobs) that still need to be
   --  executed. Return True if all have been executed, False if there is
   --  at least one job that still needs to run.
   --
   --  When this function returns True, this job can and should be scheduled.

   function Has_Dependencies (Object : Job) return Boolean is abstract;

   type Dependency_Array is array (Positive range <>) of Job_Ptr;

   procedure Set_Dependency
     (Object : access Job; Dependency : Job_Ptr) is abstract;
   --  Set a job as the dependency of the given job. Object becomes the
   --  dependent (successor) job of Dependency
   --
   --  J2.Set_Dependency (J1) creates the link J1 --> J2

   procedure Set_Dependencies
     (Object : access Job; Dependencies : Dependency_Array) is abstract;
   --  Set some jobs as dependencies of the given job. Object becomes
   --  the dependent (successor) job of each job in Dependencies
   --
   --  J4.Set_Dependencies ((J1, J2, J3)) gives:
   --
   --  J1 --
   --       \
   --  J2 ---> J4
   --       /
   --  J3 --

   procedure Chain (Jobs : Dependency_Array);
   --  Create a chain of jobs such that each job is a dependency of the
   --  next job
   --
   --  Chain ((J1, J2, J3)) will result in:
   --
   --  J1 --> J2 --> J3
   --
   --  where J1 is the first job that will be executed and J3 the last job.

   -----------------------------------------------------------------------------

   Null_Job : constant Job_Ptr;

   function Get_Null_Job return Job_Ptr is (Null_Job);
   --  A function that is used when a non-static constant (Null_Job) is
   --  needed in a preelaborated unit

   procedure Free (Pointer : in out Job_Ptr)
     with Pre => Pointer /= Null_Job;

   function Create_Empty_Job return Job_Ptr;

   -----------------------------------------------------------------------------

   type Parallel_Job is interface and Job;

   type Parallel_Job_Ptr is not null access all Parallel_Job'Class;

   procedure Set_Range (Object : in out Parallel_Job; From, To : Positive) is abstract;

   procedure Execute
     (Object   : Parallel_Job;
      Context  : Execution_Context'Class;
      From, To : Positive) is abstract;
   --  Any job which inherits Abstract_Parallel_Job needs to override this
   --  procedure instead of the regular Execute procedure

   type Parallel_Job_Cloner is not null access function
     (Job    : Parallel_Job_Ptr;
      Length : Positive) return Dependency_Array;

   function Parallelize
     (Job   : Parallel_Job_Ptr;
      Clone : Parallel_Job_Cloner;
      Length, Slice : Positive) return Job_Ptr;
   --  Parallelize a job by returning a new job that will enqueue multiple
   --  instances of the given job with different ranges
   --
   --  Length is the total number of elements and Slice is the maximum range
   --  of a slice. For example, Length => 24 and Slice => 6 will spawn 4 jobs
   --  with the ranges 1..6, 7..12, 13..18, and 19..24.

   -----------------------------------------------------------------------------

   type Abstract_Job is abstract new Job with private;

   type Abstract_Parallel_Job is abstract new Abstract_Job and Parallel_Job with private;

   overriding
   procedure Execute
     (Object  : Abstract_Parallel_Job;
      Context : Execution_Context'Class);

   overriding
   procedure Set_Range (Object : in out Abstract_Parallel_Job; From, To : Positive);

   -----------------------------------------------------------------------------

   type GPU_Job is interface and Job;

private

   type No_Job is new Job with null record;

   overriding
   procedure Execute
     (Object  : No_Job;
      Context : Execution_Context'Class);

   overriding
   function Dependent (Object : No_Job) return Job_Ptr is (Null_Job);

   overriding
   function Decrement_Dependencies (Object : in out No_Job) return Boolean is (False);

   overriding
   function Has_Dependencies (Object : No_Job) return Boolean is (False);

   overriding
   procedure Set_Dependency
     (Object : access No_Job; Dependency : Job_Ptr) is null;

   overriding
   procedure Set_Dependencies
     (Object : access No_Job; Dependencies : Dependency_Array) is null;

   Null_Job : constant Job_Ptr := new No_Job;

   -----------------------------------------------------------------------------

   subtype Zero_Counter is Atomics.Counter (Initial_Value => 0);

   type Zero_Counter_Access is access Zero_Counter;

   type Counter_Controlled is new Ada.Finalization.Controlled with record
      Counter : Zero_Counter_Access := new Zero_Counter;
   end record;

   overriding
   procedure Adjust (Object : in out Counter_Controlled);

   overriding
   procedure Finalize (Object : in out Counter_Controlled);

   type Abstract_Job is abstract new Job with record
      Dependent    : Job_Ptr := Null_Job;
      Dependencies : Counter_Controlled;
   end record;

   overriding
   function Dependent (Object : Abstract_Job) return Job_Ptr is (Object.Dependent);

   overriding
   function Decrement_Dependencies (Object : in out Abstract_Job) return Boolean;

   overriding
   function Has_Dependencies (Object : Abstract_Job) return Boolean;

   overriding
   procedure Set_Dependency
     (Object : access Abstract_Job; Dependency : Job_Ptr);

   overriding
   procedure Set_Dependencies
     (Object : access Abstract_Job; Dependencies : Dependency_Array);

   -----------------------------------------------------------------------------

   type Empty_Job is new Abstract_Job with null record;

   overriding
   procedure Execute
     (Object  : Empty_Job;
      Context : Execution_Context'Class) is null;

   function Create_Empty_Job return Job_Ptr is (new Empty_Job);

   -----------------------------------------------------------------------------

   type Parallel_For_Job is new Abstract_Job with record
      Length, Slice : Positive;
      Job   : Parallel_Job_Ptr;
      Clone : Parallel_Job_Cloner;
   end record
     with Dynamic_Predicate => not Parallel_For_Job.Job.Has_Dependencies;

   overriding
   procedure Execute
     (Object  : Parallel_For_Job;
      Context : Execution_Context'Class);

   type Abstract_Parallel_Job is abstract new Abstract_Job and Parallel_Job with record
      From, To : Positive;
   end record;

end Orka.Jobs;
