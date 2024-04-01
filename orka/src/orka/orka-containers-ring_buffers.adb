--  SPDX-License-Identifier: Apache-2.0
--
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

package body Orka.Containers.Ring_Buffers is

   function Length (Container : Buffer) return Natural is
     (Container.Count);

   function Is_Empty (Container : Buffer) return Boolean is
     (Length (Container) = 0);

   function Is_Full (Container : Buffer) return Boolean is
     (Length (Container) = Container.Capacity);

   procedure Add_Last (Container : in out Buffer; Element : Element_Type) is
   begin
      Container.Elements (Container.Head) := Element;

      Container.Head  := (Container.Head mod Container.Capacity) + 1;
      Container.Count := Container.Count + 1;
   end Add_Last;

   function Remove_First (Container : in out Buffer) return Element_Type is
      Result : constant Element_Type := Container.Elements (Container.Tail);
   begin
      --  Make position in buffer null to fix references in case Element_Type
      --  is/contains a controlled type
      Container.Elements (Container.Tail .. Container.Tail) := [others => <>];

      Container.Tail  := (Container.Tail mod Container.Capacity) + 1;
      Container.Count := Container.Count - 1;
      return Result;
   end Remove_First;

end Orka.Containers.Ring_Buffers;
