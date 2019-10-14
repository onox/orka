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

with Ada.Exceptions;
--  with Ada.Real_Time;
with Ada.Tags;
--  with Ada.Text_IO;

with Orka.Containers.Bounded_Vectors;
with Orka.Futures;
with Orka.Logging;

package body Orka.Jobs.Executors is

   use Orka.Logging;
   package Messages is new Orka.Logging.Messages (Worker);

   function Get_Root_Dependent (Element : Job_Ptr) return Job_Ptr is
      Result : Job_Ptr := Element;
   begin
      while Result.Dependent /= Null_Job loop
         Result := Result.Dependent;
      end loop;
      return Result;
   end Get_Root_Dependent;

   procedure Execute_Jobs
     (Name  : String;
      Kind  : Queues.Executor_Kind;
      Queue : Queues.Queue_Ptr)
   is
--      use type Ada.Real_Time.Time;
      use type Futures.Status;
      use Ada.Exceptions;

      Pair : Queues.Pair;
      Stop : Boolean := False;

      Null_Pair : constant Queues.Pair := (others => <>);

      package Vectors is new Orka.Containers.Bounded_Vectors (Positive, Job_Ptr);

--      T0, T1, T2 : Ada.Real_Time.Time;
   begin
      loop
--         T0 := Ada.Real_Time.Clock;
         Queue.Dequeue (Kind) (Pair, Stop);
         exit when Stop;

         declare
            Job     : Job_Ptr renames Pair.Job;

            Future  : Futures.Pointers.Reference renames Pair.Future.Get;
            Promise : Futures.Promise'Class renames Futures.Promise'Class (Future.Value.all);

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

            Tag : String renames Ada.Tags.Expanded_Name (Job'Tag);
         begin
--            T1 := Ada.Real_Time.Clock;

--            Ada.Text_IO.Put_Line (Name & " executing job " & Tag);
            if Future.Current_Status = Futures.Waiting then
               Promise.Set_Status (Futures.Running);
            end if;

            if Future.Current_Status = Futures.Running then
               begin
                  Job.Execute (Enqueue'Access);
               exception
                  when Error : others =>
                     Promise.Set_Failed (Error);

                     Messages.Log (Logging.Error,
                       Kind'Image & " job " & Tag & " " & Exception_Information (Error));
               end;
            else
               Messages.Log (Warning,
                 Kind'Image & " job " & Tag & " already " & Future.Current_Status'Image);
            end if;

--            T2 := Ada.Real_Time.Clock;
--            declare
--               Waiting_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T1 - T0);
--               Running_Time : constant Duration := 1e3 * Ada.Real_Time.To_Duration (T2 - T1);
--            begin
--               Ada.Text_IO.Put_Line (Name & " (blocked" & Waiting_Time'Image & " ms) executed job " & Tag & " in" & Running_Time'Image & " ms");
--            end;

            if Job.Dependent /= Null_Job then
               --  Make the root dependents of the jobs in Jobs
               --  dependencies of Job.Dependent
               if not Jobs.Is_Empty then
                  Set_Root_Dependent (Job.Dependent);
               end if;

               --  If another job depends on this job, decrement its dependencies counter
               --  and if it has reached zero then it can be scheduled
               if Job.Dependent.Decrement_Dependencies then
                  pragma Assert (Jobs.Is_Empty);
--                  Ada.Text_IO.Put_Line (Name & " job " & Tag & " enqueuing dependent " &
--                    Ada.Tags.Expanded_Name (Job.Dependent'Tag));
                  Queue.Enqueue (Job.Dependent, Pair.Future);
               end if;
            elsif Jobs.Is_Empty then
               Promise.Set_Status (Futures.Done);
--               Ada.Text_IO.Put_Line (Name & " completed graph with job " & Tag);
            else
               --  If the job has enqueued new jobs, we need to create an
               --  empty job which has the root dependents of these new jobs
               --  as dependencies. This is so that the empty job will be the
               --  last job that is given Pair.Future
               Set_Root_Dependent (Create_Empty_Job);
            end if;

            if not Jobs.Is_Empty then
               for Job of Jobs loop
--                  Ada.Text_IO.Put_Line (Name & " job " & Tag & " enqueuing job " &
--                    Ada.Tags.Expanded_Name (Job'Tag));
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
         Messages.Log (Logging.Error, Exception_Information (Error));
   end Execute_Jobs;

end Orka.Jobs.Executors;
