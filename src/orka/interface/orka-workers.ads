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

with Ada.Real_Time;
with Ada.Synchronous_Barriers;

with Orka.Behaviors;
with Orka.Transforms.Singles.Vectors;

generic
   Count : Positive;
   Name  : String;
   with procedure Execute
     (Scene         : Behaviors.Behavior_Array;
      Barrier       : in out Ada.Synchronous_Barriers.Synchronous_Barrier;
      Delta_Time    : Ada.Real_Time.Time_Span;
      View_Position : Transforms.Singles.Vectors.Vector4);
package Orka.Workers is

   package Transforms renames Orka.Transforms.Singles.Vectors;

   protected Barrier is
      procedure Update
        (Scene         : not null Behaviors.Behavior_Array_Access;
         Delta_Time    : Ada.Real_Time.Time_Span;
         View_Position : Transforms.Vector4);

      procedure Shutdown;

      entry Wait_For_Release
        (Scene         : out not null Behaviors.Behavior_Array_Access;
         Delta_Time    : out Ada.Real_Time.Time_Span;
         View_Position : out Transforms.Vector4;
         Shutdown      : out Boolean);
   private
      Scene : not null Behaviors.Behavior_Array_Access := Behaviors.Empty_Behavior_Array;
      DT    : Ada.Real_Time.Time_Span;
      VP    : Transforms.Vector4;

      Proceed, Ready, Stop : Boolean := False;
   end Barrier;

   type Worker is limited private;

   type Worker_Array is array (Positive range <>) of Worker;

   Workers : constant Worker_Array;

private

   task type Worker_Task (Data : not null access constant Worker);

   type Worker is limited record
      ID : Positive;
      T  : Worker_Task (Worker'Access);
   end record;

   function Make_Workers return Worker_Array;

   Workers : constant Worker_Array := Make_Workers;

end Orka.Workers;
