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

private with GL.Objects.Queries;
private with GL.Types;

package Orka.Timers is
   pragma Preelaborate;

   type State_Type is (Idle, Busy, Waiting);

   type Timer is tagged private;

   function Create_Timer return Timer;

   function State (Object : Timer) return State_Type
     with Inline;

   procedure Start (Object : in out Timer)
     with Pre  => Object.State in Idle | Waiting,
          Post => (case Object.State'Old is
                     when Idle    => Object.State = Busy,
                     when Waiting => Object.State in Waiting | Busy,
                     when Busy    => raise Program_Error);

   procedure Stop (Object : in out Timer)
     with Pre  => Object.State in Busy | Waiting,
          Post => Object.State = Waiting;

   function CPU_Duration (Object : Timer) return Duration;

   function GPU_Duration (Object : Timer) return Duration;

private

   type Timer is tagged record
      Query_Start, Query_Stop : GL.Objects.Queries.Query (GL.Objects.Queries.Timestamp);
      State        : State_Type    := Idle;
      CPU_Start    : GL.Types.Long := 0;
      CPU_Duration : Duration      := 0.0;
      GPU_Duration : Duration      := 0.0;
   end record;

   function State (Object : Timer) return State_Type is (Object.State);

end Orka.Timers;
