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

with System.Multiprocessors.Dispatching_Domains;

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Text_IO;

with Orka.Containers.Bounded_Vectors;
with Orka.Futures;
with Orka.OS;

package body Orka.Jobs.Workers is

   function Get_Root_Dependent (Element : Job_Ptr) return Job_Ptr is
      Result : Job_Ptr := Element;
   begin
      while Result.Dependent /= Null_Job loop
         Result := Result.Dependent;
      end loop;
      return Result;
   end Get_Root_Dependent;

   task body Worker_Task is
      package SM renames System.Multiprocessors;
      package SF renames Ada.Strings.Fixed;

      use type SM.CPU;
      use type Ada.Real_Time.Time;

      ID   : constant String := Positive'Image (Data.ID);
      Name : constant String := Task_Name & " #" & SF.Trim (ID, Ada.Strings.Left);

      Pair : Queues.Pair;
      Stop : Boolean := False;

      Null_Pair : constant Queues.Pair := Queues.Get_Null_Pair;

      package Vectors is new Orka.Containers.Bounded_Vectors (Job_Ptr, Get_Null_Job);

      T0, T1, T2 : Ada.Real_Time.Time;
   begin
      --  Set the CPU affinity of the task to its corresponding CPU core
      SM.Dispatching_Domains.Set_CPU (SM.CPU (Data.ID) + 1);
      Orka.OS.Set_Task_Name (Name);

      loop
         T0 := Ada.Real_Time.Clock;
         Queue.Dequeue (Queues.CPU) (Pair, Stop);
         exit when Stop;

         declare
            Job    : Job_Ptr renames Pair.Job;
            Future : Futures.Future_Access renames Pair.Future.Get;

            Jobs : Vectors.Vector (Capacity => Maximum_Enqueued_By_Job);

            procedure Enqueue (Element : Job_Ptr) is
            begin
               Jobs.Append (Element);
            end Enqueue;

            procedure Set_Root_Dependent (Last_Job : Job_Ptr) is
               Root_Dependents : Vectors.Vector (Capacity => Jobs.Length);

               procedure Set_Dependencies (Elements : Vectors.Element_Array) is
               begin
                  Last_Job.Set_Dependencies (Dependency_Array (Elements));
               end Set_Dependencies;
            begin
               for Job of Jobs loop
                  declare
                     Root : constant Job_Ptr := Get_Root_Dependent (Job);
                  begin
                     if not (for some Dependent of Root_Dependents => Root = Dependent) then
                        Root_Dependents.Append (Root);
                     end if;
                  end;
               end loop;

               Root_Dependents.Query (Set_Dependencies'Access);
            end Set_Root_Dependent;
         begin
            T1 := Ada.Real_Time.Clock;

            Future.Set_Status (Futures.Running);
            begin
               Job.Execute (Enqueue'Access);
            exception
               when others =>
                  Future.Set_Status (Futures.Failed);
                  raise;
            end;

            T2 := Ada.Real_Time.Clock;

            declare
               Waiting_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T1 - T0);
               Running_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T2 - T1);
               Tag : String renames Ada.Tags.Expanded_Name (Job'Tag);
            begin
               Ada.Text_IO.Put_Line (Name & " (blocked" & Waiting_Time'Image & " ms) executed job " & Tag & " in" & Running_Time'Image & " ms");
            end;

            if Job.Dependent /= Null_Job then
               --  Make the root dependents of the jobs in Jobs
               --  dependencies of Job.Dependent
               if not Jobs.Empty then
                  Set_Root_Dependent (Job.Dependent);
               end if;

               --  If another job depends on this job, decrement its dependencies counter
               --  and if it has reached zero then it can be scheduled
               if Job.Dependent.Decrement_Dependencies then
                  Queue.Enqueue (Job.Dependent, Pair.Future);
               end if;
            elsif Jobs.Empty then
               Future.Set_Status (Futures.Done);
            else
               --  If the job has enqueued new jobs, we need to create an
               --  empty job which has the root dependents of these new jobs
               --  as dependencies. This is so that the empty job will be the
               --  last job that is given Pair.Future
               Set_Root_Dependent (Create_Empty_Job);
            end if;

            if not Jobs.Empty then
               for Job of Jobs loop
                  Queue.Enqueue (Job, Pair.Future);
               end loop;
            end if;

            Free (Job);
         end;

         --  Finalize the smart pointer (Pair.Future) to reduce the number
         --  of references to the Future object
         Pair := Null_Pair;
      end loop;
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
   end Worker_Task;

   function Make_Workers return Worker_Array is
   begin
      return Result : Worker_Array (1 .. Positive (Count)) do
         for Index in Result'Range loop
            Result (Index).ID := Index;
         end loop;
      end return;
   end Make_Workers;

   procedure Shutdown is
   begin
      Queue.Shutdown;
   end Shutdown;

end Orka.Jobs.Workers;
