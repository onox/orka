--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

package body Orka.Timers is

   function Create_Timer return Timer is
   begin
      return Timer'(others => <>);
   end Create_Timer;

   use GL.Types;

   function Get_Duration (Value : UInt64) return Duration is
      Seconds     : constant UInt64 := Value / 1e9;
      Nanoseconds : constant UInt64 := Value - Seconds * 1e9;
   begin
      return Duration (Seconds) + Duration (Nanoseconds) / 1e9;
   end Get_Duration;

   procedure Start (Object : in out Timer) is
      use GL.Objects.Queries;
   begin
      if Object.State = Waiting and then Object.Query_Stop.Result_Available then
         declare
            Stop_Time  : constant UInt64 := Object.Query_Stop.Result;
            Start_Time : constant UInt64 := Object.Query_Start.Result;
         begin
            if Stop_Time > Start_Time then
               Object.GPU_Duration := Get_Duration (Stop_Time - Start_Time);
            end if;
         end;
         Object.State := Idle;
      end if;

      if Object.State = Idle then
         Object.CPU_Start := Get_Current_Time;
         Object.Query_Start.Record_Current_Time;
         Object.State := Busy;
      end if;
   end Start;

   procedure Stop (Object : in out Timer) is
      use GL.Objects.Queries;
   begin
      if Object.State = Busy then
         declare
            Current_Time : constant Long := Get_Current_Time;
         begin
            if Current_Time > Object.CPU_Start then
               Object.CPU_Duration := Get_Duration (UInt64 (Current_Time - Object.CPU_Start));
            end if;
         end;
         Object.Query_Stop.Record_Current_Time;
         Object.State := Waiting;
      end if;
   end Stop;

   function CPU_Duration (Object : Timer) return Duration is (Object.CPU_Duration);

   function GPU_Duration (Object : Timer) return Duration is (Object.GPU_Duration);

end Orka.Timers;
