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

with Ada.Iterator_Interfaces;

generic
   type Index_Type is new Natural;
   type Element_Type is private;
package Orka.Containers.Bounded_Vectors is
   pragma Pure;

   subtype Length_Type is Index_Type'Base range 0 .. Index_Type'Last;

   type Vector (Capacity : Length_Type) is tagged private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Reference_Type;
   pragma Preelaborable_Initialization (Vector);

   type Element_Array is array (Index_Type range <>) of aliased Element_Type;

   procedure Append_All (Container : in out Vector; Elements : Element_Array)
     with Pre  => Container.Length + Elements'Length <= Container.Capacity,
          Post => Container.Length = Container'Old.Length + Elements'Length;

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

   procedure Clear (Container : in out Vector)
     with Post => Container.Length = 0;

   procedure Query
     (Container : Vector;
      Process   : not null access procedure (Elements : Element_Array));

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type))
   with Pre => Index <= Container.Length;

   -----------------------------------------------------------------------------

   function Element (Container : Vector; Index : Index_Type) return Element_Type;

   type Constant_Reference_Type (Value : not null access constant Element_Type) is limited private
     with Implicit_Dereference => Value;

   type Reference_Type (Value : not null access Element_Type) is limited private
     with Implicit_Dereference => Value;

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type) return Constant_Reference_Type;

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type) return Reference_Type;

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean is
     (Position /= No_Element);

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Vector)
     return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   -----------------------------------------------------------------------------

   function Length (Container : Vector) return Length_Type
     with Inline;

   function Is_Empty (Container : Vector) return Boolean
     with Inline;

   function Is_Full (Container : Vector) return Boolean
     with Inline;

private

   type Vector_Access is access constant Vector;

   type Cursor is record
      Object : Vector_Access;
      Index  : Length_Type;
   end record;

   No_Element : constant Cursor := Cursor'(null, 0);

   -----------------------------------------------------------------------------

   function Element
     (Container : aliased Vector;
      Position  : Cursor) return Element_Type;

   type Constant_Reference_Type
     (Value : not null access constant Element_Type) is limited null record;

   type Reference_Type (Value : not null access Element_Type) is limited null record;

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor) return Constant_Reference_Type;

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor) return Reference_Type;

   -----------------------------------------------------------------------------

   type Vector (Capacity : Length_Type) is tagged record
      Elements : Element_Array (Index_Type'First .. Capacity) := (others => <>);
      Length   : Length_Type := 0;
   end record;
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
