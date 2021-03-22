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

with Orka.Jobs.System;
with Orka.Resources.Loader;

package Orka_Package_KTX is

   package Job_System is new Orka.Jobs.System
     (Maximum_Queued_Jobs => 16,
      Maximum_Job_Graphs  => 4);

   package Loader is new Orka.Resources.Loader
     (Job_System.Queues, Job_System.Queue'Unchecked_Access, Maximum_Requests => 10);

end Orka_Package_KTX;
