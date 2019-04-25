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

private with Ada.Iterator_Interfaces;

generic
   type Element_Type is private;
package Orka.Containers.Bounded_Vectors is
   pragma Preelaborate;

   subtype Index_Type is Positive;

   type Vector (Capacity : Positive) is tagged private
     with Constant_Indexing => Element,
          Variable_Indexing => Reference;
   pragma Preelaborable_Initialization (Vector);

   procedure Append (Container : in out Vector; Elements : Vector)
     with Pre  => Container.Length + Elements.Length <= Container.Capacity,
          Post => Container.Length = Container'Old.Length + Elements.Length;

   procedure Append (Container : in out Vector; Element : Element_Type)
     with Pre  => Container.Length < Container.Capacity,
          Post => Container.Length = Container'Old.Length + 1;
   --  Add the element to the end of the vector

   procedure Remove_Last (Container : in out Vector; Element : out Element_Type)
     with Pre  => Container.Length > 0,
          Post => Container.Length = Container'Old.Length - 1;

   type Element_Array is array (Index_Type range <>) of aliased Element_Type;

   procedure Query
     (Container : Vector;
      Process   : not null access procedure (Elements : Element_Array));

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type))
   with Pre => Index <= Container.Length;

   function Element (Container : Vector; Index : Index_Type) return Element_Type;

   type Reference_Type (Value : not null access Element_Type) is limited private
     with Implicit_Dereference => Value;

   function Reference
     (Container : in out Vector;
      Index     : Index_Type) return Reference_Type;

   function Length (Container : Vector) return Natural
     with Inline;

   function Empty (Container : Vector) return Boolean
     with Inline;

   function Full (Container : Vector) return Boolean
     with Inline;

private

   type Vector_Access is access constant Vector;

   type Cursor is record
      Object : Vector_Access;
      Index  : Natural;
   end record;

   No_Element : constant Cursor := Cursor'(null, 0);

   -----------------------------------------------------------------------------

   function Element
     (Container : aliased Vector;
      Position  : Cursor) return Element_Type;

   type Reference_Type (Value : not null access Element_Type) is limited null record;

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor) return Reference_Type;

   -----------------------------------------------------------------------------

   function Has_Element (Position : Cursor) return Boolean is
     (Position /= No_Element);

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Vector)
     return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   type Vector (Capacity : Positive) is tagged record
      Elements : Element_Array (1 .. Capacity) := (others => <>);
      Length   : Natural  := 0;
   end record
     with Default_Iterator  => Iterate,
          Iterator_Element  => Element_Type;
   pragma Preelaborable_Initialization (Vector);

   -----------------------------------------------------------------------------

   type Iterator is limited new
     Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end Orka.Containers.Bounded_Vectors;
