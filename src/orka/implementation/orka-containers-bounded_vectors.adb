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

package body Orka.Containers.Bounded_Vectors is

   function Length (Container : Vector) return Natural is
     (Container.Length);

   function Empty (Container : Vector) return Boolean is
     (Length (Container) = 0);

   function Full (Container : Vector) return Boolean is
     (Length (Container) = Container.Capacity);

   procedure Append (Container : in out Vector; Element : Element_Type) is
   begin
      Container.Length := Container.Length + 1;
      Container.Elements (Container.Length) := Element;
   end Append;

   procedure Query
     (Container : Vector;
      Process   : not null access procedure (Elements : Element_Array)) is
   begin
      Process (Container.Elements (1 .. Container.Length));
   end Query;

   function Element (Container : Vector; Index : Positive) return Element_Type is
     (Container.Elements (Index));

   function Element (Container : aliased Vector; Position : Cursor) return Element_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Object /= Container'Access then
         raise Program_Error;
      else
         return Element (Position.Object.all, Position.Index);
      end if;
   end Element;

   function Iterate (Container : Vector)
     return Vector_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterator'(Limited_Controlled with Container => Container'Unrestricted_Access);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Empty then
         return No_Element;
      else
         return Cursor'(Object => Object.Container, Index => 1);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Empty then
         return No_Element;
      else
         return Cursor'(Object => Object.Container,
                        Index  => Object.Container.all.Length);
      end if;
   end Last;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Index = Position.Object.Length then
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

end Orka.Containers.Bounded_Vectors;
