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

with Ada.Unchecked_Deallocation;

package body Orka.Jobs is

   procedure Free (Pointer : in out Job_Ptr) is
      type Job_Access is access all Job'Class;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Job'Class, Name => Job_Access);
   begin
      Free (Job_Access (Pointer));
   end Free;

   overriding
   function Decrement_Dependencies (Object : in out Abstract_Job) return Boolean is
      use type Atomics.Unsigned_32;
   begin
      return Atomics.Decrement (Object.Dependencies) = 0;
   end Decrement_Dependencies;

   procedure Set_Dependencies (Object : Job_Ptr; Dependencies : Dependency_Array) is
   begin
      Abstract_Job (Object.all).Dependencies := Dependencies'Length;
      for Dependency of Dependencies loop
         Abstract_Job (Dependency.all).Dependent := Object;
      end loop;
   end Set_Dependencies;

end Orka.Jobs;
