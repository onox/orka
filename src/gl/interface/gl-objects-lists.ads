--------------------------------------------------------------------------------
-- Copyright (c) 2014, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Iterator_Interfaces;

private with Ada.Finalization;

generic
   type Object_Type (<>) is new GL_Object with private;
   with function Generate_From_Id (Id : UInt) return Object_Type;
package GL.Objects.Lists is
   pragma Preelaborate;

   type List (<>) is tagged private
      with Default_Iterator  => Iterate,
           Iterator_Element  => Object_Type,
           Constant_Indexing => Element_Value;

   type Cursor is private;

   No_Element : constant Cursor;

   function Create (Raw : UInt_Array) return List;

   function Element (Position : Cursor) return Object_Type;
   function Element_Value (Container : aliased List; Position : Cursor) return Object_Type;

   function Has_Element (Position : Cursor) return Boolean is
     (Position /= No_Element);

   package List_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : List)
     return List_Iterator_Interfaces.Reversible_Iterator'Class;

private
   type List (Count : Size) is tagged record
      Contents : UInt_Array (1 .. Count);
   end record;

   type List_Access is access constant List;

   type Cursor is record
      Object : List_Access;
      Index  : Size;
   end record;

   No_Element : constant Cursor := Cursor'(null, 0);

   use Ada.Finalization;

   type Iterator is new Limited_Controlled and
     List_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : List_Access;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end GL.Objects.Lists;
