--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

generic
   type Element_Type is private;
   with function Null_Element return Element_Type;
package Orka.Containers.Ring_Buffers is
   pragma Preelaborate;

   type Buffer (Capacity : Positive) is tagged private;

   procedure Add_Last (Container : in out Buffer; Element : Element_Type)
     with Pre  => Container.Length < Container.Capacity,
          Post => Container.Length = Container'Old.Length + 1;
   --  Add the element to the end of the buffer (at index k + 1)

   function Remove_First (Container : in out Buffer) return Element_Type
     with Pre  => Container.Length > 0,
          Post => Container.Length = Container'Old.Length - 1;
   --  Remove and return the first element in the buffer (at index 1)

   function Length (Container : Buffer) return Natural;

   function Empty (Container : Buffer) return Boolean;

   function Full (Container : Buffer) return Boolean;

private

   type Element_Array is array (Positive range <>) of Element_Type;

   type Buffer (Capacity : Positive) is tagged record
      Elements   : Element_Array (1 .. Capacity) := (others => Null_Element);
      Head, Tail : Positive := 1;
      Count      : Natural  := 0;
   end record;

end Orka.Containers.Ring_Buffers;
