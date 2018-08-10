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

package body Orka.Resources.Managers is

   function Contains (Object : Manager; Name : String) return Boolean is
     (Object.Resources.Contains (Name));

   function Resource (Object : Manager; Name : String) return Resource_Ptr is
     (Object.Resources.Element (Name));

   procedure Add_Resource
     (Object   : in out Manager;
      Name     : String;
      Resource : Resource_Ptr) is
   begin
      Resource_Maps.Insert (Object.Resources, Name, Resource);
   end Add_Resource;

   procedure Remove_Resource (Object : in out Manager; Name : String) is
   begin
      Resource_Maps.Delete (Object.Resources, Name);
      --  TODO Free the resource (Batch, other GL objects etc.) in a GPU job
   end Remove_Resource;

   function Create_Manager return Manager_Ptr is (new Manager);

end Orka.Resources.Managers;
