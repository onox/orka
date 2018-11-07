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

with Ada.Containers.Indefinite_Holders;

with Orka.Rendering.Buffers.Persistent_Mapped;
with Orka.Types;

generic
   type Partition_Index_Type is mod <>;
package Orka.Instances is
   pragma Preelaborate;

   type Manager is tagged limited private;

   function Create_Manager (Capacity, Parts : Positive) return Manager;
   --  Return a manager that manages a persistent mapped buffer with
   --  Capacity * Parts transforms.

   type Cursor is private;

   function Add_Instance (Object : in out Manager) return Cursor
     with Pre  => Object.Length < Object.Capacity,
          Post => Object.Length'Old + 1 = Object.Length;

   procedure Remove_Instance (Object : in out Manager; Instance : Cursor)
     with Pre  => Object.Length > 0,
          Post => Object.Length'Old - 1 = Object.Length;

   procedure Set_Transform
     (Object   : in out Manager;
      Value    : Orka.Types.Singles.Matrix4;
      Instance : Cursor;
      Part     : Natural)
   with Pre => Part < Object.Parts;

   function Transforms (Object : Manager) return Rendering.Buffers.Bindable_Buffer'Class;

   function Parts (Object : Manager) return Positive;
   --  Return number of parts of each instance

   function Length (Object : Manager) return Natural;
   --  Return current number of instances

   function Capacity (Object : Manager) return Positive;
   --  Return maximum number of instances

   procedure Complete_Frame (Object : in out Manager);

private

   package PMB is new Orka.Rendering.Buffers.Persistent_Mapped (Partition_Index_Type);

   type Cursor is new Positive;

   subtype Instance is Positive;
   subtype Index is Positive;

   type Instance_Array is array (Index    range <>) of Instance;
   type Indices_Array  is array (Instance range <>) of Index;

   type Instances_Type (Capacity : Positive) is record
      Instances : Instance_Array (1 .. Capacity);
      Indices   : Indices_Array  (1 .. Capacity);
   end record;
   --  Instances is an array containing a contiguous number of instance
   --  ID's that have been added, followed by a contiguous number of ID's
   --  that have been removed. This array is used to quickly determine a
   --  free ID that can be returned by the function Add_Instance.
   --
   --  Indices is an array that is used to look up the position of an
   --  instance ID in Instances in O(1) time. Iterating over Instances
   --  would be an O(n) operation.
   --
   --  For example:
   --
   --  Instances:
   --
   --  1 3 4 | 2 5
   --  -----   ---
   --  added   removed
   --
   --  Indices:
   --
   --  1 4 2 3 5
   --
   --  To add an instance, the '|' is moved to the right and the ID next
   --  to it (2 in the example above) is returned.
   --
   --  To remove an instance, we look up the index of the instance and
   --  then swap its ID with the instance at the end of the array of
   --  added instances. Finally, we move the '|' to the left.
   --
   --  The indices of the two swapped instances are updated in Indices.
   --
   --  For example, given the above array, removing instance 1 results in:
   --
   --    4 3 | 1 2 5
   --    ---   -----
   --  added   removed

   package Instances_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => Instances_Type);

   type Manager is tagged limited record
      Added : Natural := 0;
      --  Represents the '|' barrier between the added and removed instances

      Capacity, Parts : Positive;

      Instances : Instances_Holder.Holder;

      Transforms : PMB.Persistent_Mapped_Buffer
        (Kind => Orka.Types.Single_Matrix_Type,
         Mode => PMB.Write);
   end record;

end Orka.Instances;
