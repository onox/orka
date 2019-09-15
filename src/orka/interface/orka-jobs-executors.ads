--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Orka.Jobs.Queues;

generic
   with package Queues is new Orka.Jobs.Queues (<>);

   Maximum_Enqueued_By_Job : Positive;
   --  Maximum number of extra jobs that a job can enqueue
package Orka.Jobs.Executors is

   procedure Execute_Jobs
     (Name  : String;
      Kind  : Queues.Executor_Kind;
      Queue : Queues.Queue_Ptr);

end Orka.Jobs.Executors;
