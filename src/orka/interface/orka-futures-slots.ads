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

generic
   Count : Positive;
package Orka.Futures.Slots is

   subtype Slot_Index is Positive range 1 .. Count;

   subtype Future_Handle is Slot_Index;

   subtype Location_Index is Slot_Index;

   protected type Future_Object is new Futures.Future with
      overriding
      function Current_Status return Futures.Status;

      overriding
      procedure Set_Status (Value : Futures.Status);

      overriding
      entry Wait_Until_Done (Value : out Futures.Status);

      function Handle_Location return Location_Index;

      procedure Reset_And_Set_Location (Value : Location_Index);
   private
      Status   : Futures.Status := Futures.Waiting;
      Location : Location_Index := 1;
   end Future_Object;

   -----------------------------------------------------------------------------

   type Handle_Array is array (Location_Index) of Future_Handle;

   function Make_Handles return Handle_Array;

   type Future_Object_Access is access all Future_Object;

   protected Manager is
      entry Acquire (Slot : out Future_Object_Access);
      --  with Pre => not Stopping

      procedure Release (Slot : not null Future_Object_Access);
        --with Pre => Stopping or else Slot.Current_Status in Futures.Done | Futures.Failed;

      procedure Shutdown;

      function Acquired_Slots return Natural;

      function Stopping return Boolean;
   private
      Handles : Handle_Array := Make_Handles;
      --  An array containing a contiguous number of handles pointing to
      --  acquired Future_Object objects in the Future_Slots variable and
      --  then followed by a contiguous number of handles pointing to
      --  released Future_Object objects.
      --
      --  For example:
      --
      --     1 3 4 | 2 5
      --     -----   ---
      --  acquired   released
      --
      --  To acquire a slot, we simply move the '|' between the two arrays
      --  to the right and return the Future_Object object to which the
      --  new handle is pointing. In the example we return
      --  Future_Slots (2)'Access.
      --
      --  To release a slot, we lookup the location (which is stored in the
      --  Future_Object object itself) of its handle and then swap the
      --  handle with the handle at the end of the array of acquired slots.
      --  Finally, we move the '|' to the left.
      --
      --  For example, given the above array, releasing slot 1 results in:
      --
      --       4 3 | 1 2 5
      --       ---   -----
      --  acquired   released
      --
      --  The two Future_Object objects of which their handles were
      --  swapped (slots 1 and 4) are updated to point to the new location
      --  containing their handle.

      Acquired : Natural := 0;
      --  Represents the '|' barrier between the acquired slots and
      --  released slots

      Should_Stop : Boolean := False;
   end Manager;

private

   type Future_Array is array (Future_Handle) of aliased Future_Object;

   function Make_Futures return Future_Array;

   Future_Slots : Future_Array := Make_Futures;

end Orka.Futures.Slots;
