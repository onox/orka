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

with Ada.Real_Time;

with Orka.Containers.Ring_Buffers;
with Orka.Futures;
with Orka.Jobs.Queues;

generic
   Maximum_Requests : Positive;
   --  Maximum number of resources waiting to be read from a file system
   --  or archive. Resources are read sequentially, but may be processed
   --  concurrently. This number depends on how fast the hardware can read
   --  the requested resources.

   Maximum_Processing : Positive;
   --  Maximum number of resources that may be processed concurrently

   Job_Queue : Jobs.Queues.Queue_Ptr;
   Name      : String := "Resource Loader";
   --  TODO Default_Ceiling priority?
package Orka.Resources.Loader is

   function Load (Path : String) return Futures.Pointers.Constant_Reference;
   --  Load the given resource from a file system or archive and return
   --  a handle for querying the processing status of the resource. Calling
   --  this function may block until there is a free slot available for
   --  processing the data.

   procedure Shutdown;

private

   type Processor_Ptr is not null access procedure
     (Bytes  : in out Byte_Array_Access;
      Time   : Ada.Real_Time.Time_Span;
      Path   : SU.Unbounded_String;
      Queue  : Jobs.Queues.Queue_Ptr;
      Future : Futures.Pointers.Pointer);

   procedure Empty_Processor
     (Bytes  : in out Byte_Array_Access;
      Time   : Ada.Real_Time.Time_Span;
      Path   : SU.Unbounded_String;
      Queue  : Jobs.Queues.Queue_Ptr;
      Future : Futures.Pointers.Pointer) is null;
   --  This null procedure is needed because without it the declaration
   --  of a Resource_Read_Request in the task body will generate an access
   --  check failure.

   type Resource_Read_Request is record
      Path      : SU.Unbounded_String;
      Processor : Processor_Ptr := Empty_Processor'Access;
      Future    : Futures.Pointers.Pointer;
   end record;

   Null_Request : constant Resource_Read_Request := (others => <>);

   function Get_Null_Request return Resource_Read_Request is (Null_Request);

   package Buffers is new Orka.Containers.Ring_Buffers
     (Resource_Read_Request, Get_Null_Request);

   protected Queue is
      entry Enqueue (Element : Resource_Read_Request);

      entry Dequeue (Element : out Resource_Read_Request; Stop : out Boolean);

      procedure Shutdown;
   private
      Requests    : Buffers.Buffer (Maximum_Requests);
      Should_Stop : Boolean := False;
   end Queue;

   task Loader;

   -----------------------------------------------------------------------------

   subtype Slot_Index is Positive range 1 .. Maximum_Processing;

   subtype Future_Handle is Slot_Index;

   subtype Location_Index is Slot_Index;

   protected type Future_Resource is new Futures.Future with
      overriding
      function Current_Status return Futures.Status;

      overriding
      procedure Set_Status (Value : Futures.Status);

      overriding
      entry Wait_Until_Done (Value : out Futures.Status);

      function Handle_Location return Location_Index;

      procedure Set_Location (Value : Location_Index);
   private
      Status   : Futures.Status := Futures.Waiting;
      Location : Location_Index := 1;
   end Future_Resource;

   type Future_Array is array (Future_Handle) of aliased Future_Resource;

   function Make_Futures return Future_Array;

   Processing_Slots : Future_Array := Make_Futures;

   -----------------------------------------------------------------------------

   type Handle_Array is array (Location_Index) of Future_Handle;

   function Make_Handles return Handle_Array;

   type Future_Resource_Access is access all Future_Resource;

   protected Slots is
      entry Acquire (Slot : out Future_Resource_Access);

      procedure Release (Slot : not null Future_Resource_Access)
        with Pre => Slot.Current_Status in Futures.Done | Futures.Failed;
   private
      Handles : Handle_Array := Make_Handles;
      --  An array containing a contiguous number of handles pointing to
      --  acquired Future_Resource objects in the Processing_Slots
      --  variable and then followed by a contiguous number of handles
      --  pointing to released Future_Resource objects.
      --
      --  For example:
      --
      --     1 3 4 | 2 5
      --     -----   ---
      --  acquired   released
      --
      --  To acquire a slot, we simply move the '|' between the two arrays
      --  to the right and return the Future_Resource object to which the
      --  new handle is pointing. In the example we return
      --  Processing_Slots (2)'Access.
      --
      --  To release a slot, we lookup the location (which is stored in the
      --  Future_Resource object itself) of its handle and then swap the
      --  handle with the handle at the end of the array of acquired slots.
      --  Finally, we move the '|' to the left.
      --
      --  For example, given the above array, releasing slot 1 results in:
      --
      --       4 3 | 1 2 5
      --       ---   -----
      --  acquired   released
      --
      --  The two Future_Resource objects of which their handles were
      --  swapped (slots 1 and 4) are updated to point to the new location
      --  containing their handle.

      Acquired : Natural := 0;
      --  Represents the '|' barrier between the acquired slots and
      --  released slots
   end Slots;

end Orka.Resources.Loader;
