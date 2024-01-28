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

package body Orka.Containers.Bounded_Vectors is

   function Length (Container : Vector) return Length_Type is
     (Container.Length);

   function Is_Empty (Container : Vector) return Boolean is
     (Length (Container) = 0);

   function Is_Full (Container : Vector) return Boolean is
     (Length (Container) = Container.Capacity);

   procedure Append (Container : in out Vector; Elements : Vector) is
      Start_Index : constant Index_Type := Container.Length + Index_Type'First;
      Stop_Index  : constant Index_Type'Base := Start_Index + Elements.Length - 1;

      procedure Copy_Elements (Elements : Element_Array) is
      begin
         Container.Elements (Start_Index .. Stop_Index) := Elements;
      end Copy_Elements;
   begin
      if not Elements.Is_Empty then
         Elements.Query (Copy_Elements'Access);
         Container.Length := Container.Length + Elements.Length;
      end if;
   end Append;

   procedure Append (Container : in out Vector; Element : Element_Type) is
      Index : constant Index_Type := Container.Length + Index_Type'First;
   begin
      Container.Length := Container.Length + 1;
      Container.Elements (Index) := Element;
   end Append;

   procedure Remove_Last (Container : in out Vector; Element : out Element_Type) is
      Index : constant Index_Type := Container.Length + Index_Type'First - 1;
   begin
      Element := Container.Elements (Index);
      Container.Elements (Index .. Index) := (others => <>);
      Container.Length := Container.Length - 1;
   end Remove_Last;

   procedure Clear (Container : in out Vector) is
   begin
      Container.Elements := (others => <>);
      Container.Length   := 0;
   end Clear;

   procedure Query
     (Container : Vector;
      Process   : not null access procedure (Elements : Element_Array))
   is
      Last_Index : constant Index_Type'Base := Container.Length + Index_Type'First - 1;
   begin
      Process (Container.Elements (Index_Type'First .. Last_Index));
   end Query;

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Process   : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Container.Elements (Index));
   end Update;

   function Element (Container : Vector; Index : Index_Type) return Element_Type is
     (Container.Elements (Index));

   function Element (Container : aliased Vector; Position : Cursor) return Element_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Object.all /= Container then
         raise Program_Error;
      else
         return Element (Container, Position.Index);
      end if;
   end Element;

   function Constant_Reference
     (Container : aliased Vector;
      Index     : Index_Type) return Constant_Reference_Type is
   begin
      return Constant_Reference_Type'(Value => Container.Elements (Index)'Access);
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Vector;
      Position  : Cursor) return Constant_Reference_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Object.all /= Container then
         raise Program_Error;
      else
         return Constant_Reference (Container, Position.Index);
      end if;
   end Constant_Reference;

   function Reference
     (Container : aliased in out Vector;
      Index     : Index_Type) return Reference_Type is
   begin
      return Reference_Type'(Value => Container.Elements (Index)'Access);
   end Reference;

   function Reference
     (Container : aliased in out Vector;
      Position  : Cursor) return Reference_Type is
   begin
      if Position = No_Element then
         raise Constraint_Error;
      elsif Position.Object.all /= Container then
         raise Program_Error;
      else
         return Reference (Container, Position.Index);
      end if;
   end Reference;

   function Iterate (Container : Vector)
     return Vector_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Iterator'(Container => Container'Unchecked_Access);
   end Iterate;

   overriding function First (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Is_Empty then
         return No_Element;
      else
         return Cursor'(Object => Object.Container, Index => Index_Type'First);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor is
   begin
      if Object.Container.all.Is_Empty then
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
      elsif Position.Index = Position.Object.Length + Index_Type'First - 1 then
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
      elsif Position.Index = Index_Type'First then
         return No_Element;
      else
         return Cursor'(Position.Object, Position.Index - 1);
      end if;
   end Previous;

end Orka.Containers.Bounded_Vectors;
