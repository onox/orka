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

with Ada.Exceptions;

with Orka.Smart_Pointers;

package Orka.Futures is
   pragma Preelaborate;

   type Status is (Waiting, Running, Done, Failed)
     with Default_Value => Waiting;

   subtype Non_Failed_Status is Status range Waiting .. Done;

   type Future is synchronized interface;

   procedure Wait_Until_Done
     (Object : in out Future;
      Value  : out Status) is abstract
   with Synchronization => By_Entry;

   function Current_Status (Object : Future) return Status is abstract;

   type Future_Access is access all Future'Class;

   package Pointers is new Orka.Smart_Pointers (Future'Class, Future_Access);

   -----------------------------------------------------------------------------

   type Promise is synchronized interface and Future;

   procedure Set_Status
     (Object : in out Promise;
      Value  : Non_Failed_Status) is abstract
   with Synchronization => By_Protected_Procedure;

   procedure Set_Failed
     (Object : in out Promise;
      Reason : Ada.Exceptions.Exception_Occurrence) is abstract
   with Synchronization => By_Protected_Procedure;

end Orka.Futures;
