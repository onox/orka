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

--  Based on Ada gem #97 [1] and #107 [2]
--
--  [1] https://www.adacore.com/gems/gem-97-reference-counting-in-ada-part-1
--  [2] https://www.adacore.com/gems/gem-107-preventing-deallocation-for-reference-counted-types

private with Ada.Finalization;

private with Orka.Atomics;

generic
   type Object_Type is private;
   --  Objects are freed by providing an access to a procedure in Set. This
   --  is needed if Object_Type is a classwide type and we don't know how
   --  to free an object when this package gets instantiated.
package Orka.Smart_Pointers is
   pragma Preelaborate;

   type Free_Ptr is not null access procedure (Value : in out Object_Type);

   type Reference (Value : not null access Object_Type) is limited private
     with Implicit_Dereference => Value;

   type Constant_Reference (Value : not null access constant Object_Type) is limited private
     with Implicit_Dereference => Value;

   type Abstract_Pointer is abstract tagged private;

   function Is_Null (Object : Abstract_Pointer) return Boolean;

   function References (Object : Abstract_Pointer) return Natural
     with Pre => not Object.Is_Null;

   procedure Set
     (Object : in out Abstract_Pointer;
      Value  : Object_Type;
      Free   : not null access procedure (Value : in out Object_Type))
   with Post => not Object.Is_Null and then Object.References = 1;

   type Mutable_Pointer is new Abstract_Pointer with private;

   function Get (Object : Mutable_Pointer) return Reference
     with Pre => not Object.Is_Null;

   type Pointer is new Abstract_Pointer with private;

   function Get (Object : Pointer) return Constant_Reference
     with Pre => not Object.Is_Null;

private

   type Data_Record is record
      References : Atomics.Unsigned_32 := 0;
      Object     : aliased Object_Type;
      Free       : Free_Ptr;
   end record;

   type Data_Record_Access is access Data_Record;

   type Abstract_Pointer is abstract new Ada.Finalization.Controlled with record
      Data : Data_Record_Access;
   end record;

   overriding
   procedure Adjust (Object : in out Abstract_Pointer);

   overriding
   procedure Finalize (Object : in out Abstract_Pointer);

   type Mutable_Pointer is new Abstract_Pointer with null record;

   type Pointer is new Abstract_Pointer with null record;

   type Reference (Value : not null access Object_Type) is limited record
      Hold : Mutable_Pointer;
   end record;

   type Constant_Reference (Value : not null access constant Object_Type) is limited record
      Hold : Pointer;
   end record;

end Orka.Smart_Pointers;
