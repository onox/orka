--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Interfaces.C.Pointers;

package GL.Types.Indirect is
   pragma Preelaborate;

   type Single_Array_Access is access constant Single_Array;
   type UInt_Array_Access   is access constant UInt_Array;

   type Arrays_Indirect_Command is record
      Count, Instances, First_Vertex, Base_Instance : UInt;
   end record;

   type Elements_Indirect_Command is record
      Count, Instances, First_Index, Base_Vertex, Base_Instance : UInt;
   end record;

   type Arrays_Indirect_Command_Array is array (Size range <>)
     of aliased Arrays_Indirect_Command
     with Convention => C;

   type Elements_Indirect_Command_Array is array (Size range <>)
     of aliased Elements_Indirect_Command
     with Convention => C;

   package Arrays_Indirect_Command_Pointers is new Interfaces.C.Pointers
     (Size, Arrays_Indirect_Command,
      Arrays_Indirect_Command_Array, Arrays_Indirect_Command'(others => 0));

   package Elements_Indirect_Command_Pointers is new Interfaces.C.Pointers
     (Size, Elements_Indirect_Command,
      Elements_Indirect_Command_Array, Elements_Indirect_Command'(others => 0));

end GL.Types.Indirect;
