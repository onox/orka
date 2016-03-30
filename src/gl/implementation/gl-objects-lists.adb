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

package body GL.Objects.Lists is

   function Create (Raw : UInt_Array) return List is
   begin
      return List'(Count => Raw'Length, Contents => Raw);
   end Create;

   overriding function First (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Count = 0 then
         return No_Element;
      else
         return Cursor'(Object => Object.Container, Index => 1);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Count = 0 then
         return No_Element;
      else
         return Cursor'(Object => Object.Container,
                        Index  => Object.Container.all.Contents'Length);
      end if;
   end Last;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Index = Position.Object.Contents'Length then
         return No_Element;
      else
         return Cursor'(Position.Object, Position.Index + 1);
      end if;
   end Next;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Index = 1 then
         return No_Element;
      else
         return Cursor'(Position.Object, Position.Index - 1);
      end if;
   end Previous;

   function Element (Position : Cursor) return Object_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      else
         return Element_Value (Position.Object.all, Position);
      end if;
   end Element;

   function Element_Value (Container : aliased List; Position : Cursor) return Object_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Object /= Container'Access then
         raise Program_Error;
      else
         return Generate_From_Id (Container.Contents (Position.Index));
      end if;
   end Element_Value;

   function Iterate (Container : List)
     return List_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterator'(Limited_Controlled with Container => Container'Unrestricted_Access);
   end Iterate;

end GL.Objects.Lists;
