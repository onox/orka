--  Copyright (c) 2014 Felix Krause <contact@flyx.org>
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
      return Iterator'(Limited_Controlled with Container => Container'Unchecked_Access);
   end Iterate;

end GL.Objects.Lists;
