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

   procedure Start (Object : in out Timer) is
      use GL.Objects.Queries;
   begin
      if Object.State = Idle then
         Object.Query_Start.Record_Current_Time;
         Object.CPU_Start := Get_Current_Time;
         Object.State := Busy;
      end if;
   end Start;

   procedure Stop (Object : in out Timer) is
      use GL.Objects.Queries;
      use type GL.Types.Long;
   begin
      if Object.State = Busy then
         Object.Query_Stop.Record_Current_Time;
         Object.CPU_Duration := Duration (Get_Current_Time - Object.CPU_Start) / 1.0e9;
         Object.State := Waiting;
      end if;
   end Stop;

   function CPU_Duration (Object : Timer) return Duration is (Object.CPU_Duration);

   function GPU_Duration (Object : in out Timer) return Duration is
      use GL.Types;
   begin
      if Object.State = Waiting and then Object.Query_Stop.Result_Available then
         Object.GPU_Duration :=
           Duration (UInt64'(Object.Query_Stop.Result - Object.Query_Start.Result)) / 1.0e9;
         Object.State := Idle;
      end if;
      return Object.GPU_Duration;
   end GPU_Duration;

end Orka.Timers;
