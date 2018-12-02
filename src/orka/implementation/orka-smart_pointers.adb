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

with Ada.Unchecked_Deallocation;

package body Orka.Smart_Pointers is

   procedure Free is new Ada.Unchecked_Deallocation (Data_Record, Data_Record_Access);

   function Is_Null (Object : Abstract_Pointer) return Boolean is (Object.Data = null);

   function References (Object : Abstract_Pointer) return Natural is
     (Object.Data.References.Count);

   procedure Set
     (Object : in out Abstract_Pointer;
      Value  : Object_Access) is
   begin
      if Object.Data /= null then
         --  Decrement old reference count
         Finalize (Object);
      end if;

      Object.Data := new Data_Record'(Object => Value, References => <>);
   end Set;

   function Get (Object : Mutable_Pointer) return Reference is
   begin
      return Reference'(Value => Object.Data.Object, Hold => Object);
   end Get;

   function Get (Object : Pointer) return Constant_Reference is
   begin
      return Constant_Reference'(Value => Object.Data.Object, Hold => Object);
   end Get;

   overriding
   procedure Adjust (Object : in out Abstract_Pointer) is
   begin
      if Object.Data /= null then
         Object.Data.References.Increment;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Abstract_Pointer) is
      Zero : Boolean;
   begin
      if Object.Data /= null then
         Object.Data.References.Decrement (Zero);
         if Zero then
            Free_Object (Object.Data.Object);
            Free (Object.Data);
         end if;
      end if;

      --  Idempotence: next call to Finalize has no effect
      Object.Data := null;
   end Finalize;

end Orka.Smart_Pointers;
