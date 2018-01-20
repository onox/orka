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

with Ada.Exceptions;
with Ada.Text_IO;

with Orka.OS;
with Orka.Resources.Models.glTF;
with Orka.Resources.Textures.KTX;

package body Orka.Resources.Loader is

   procedure Free_Future (Value : in out Futures.Future_Access) is
   begin
      Slots.Release (Future_Resource_Access (Value));
   end Free_Future;

   function Is_Extension (Path, Extension : String) return Boolean is
     (Path (Path'Last - Extension'Length .. Path'Last) = "." & Extension);

   function Load (Path : String) return Futures.Pointers.Pointer is
      function Processor return Processor_Ptr is
      begin
         if Is_Extension (Path, "gltf") then
            return Orka.Resources.Models.glTF.Process'Access;
         elsif Is_Extension (Path, "ktx") then
            return Orka.Resources.Textures.KTX.Process'Access;
         else
            raise Resource_Load_Error with Path & " has an unknown extension";
         end if;
      end Processor;

      Future : Future_Resource_Access;
   begin
      Slots.Acquire (Future);
      declare
         Pointer : Futures.Pointers.Pointer;
      begin
         Pointer.Set (Futures.Future_Access (Future), Free_Future'Access);
         Queue.Enqueue
           ((Path      => SU.To_Unbounded_String (Path),
             Processor => Processor,
             Future    => Pointer));
         return Pointer;
         --  Return Pointer instead of Pointer.Get to prevent
         --  adjust/finalize raising a Storage_Error
      end;
   end Load;

   function Load (Path : String) return Futures.Pointers.Constant_Reference is
     (Load (Path).Get);
   --  A helper function to avoid raising a Storage_Error

   protected body Queue is
      entry Enqueue (Element : Resource_Read_Request) when not Requests.Full is
      begin
         Requests.Add_Last (Element);
      end Enqueue;

      entry Dequeue (Element : out Resource_Read_Request; Stop : out Boolean)
        when Should_Stop or not Requests.Empty is
      begin
         Stop := Should_Stop;
         if Should_Stop then
            return;
         end if;

         Element := Requests.Remove_First;
      end Dequeue;

      procedure Shutdown is
      begin
         Should_Stop := True;
      end Shutdown;
   end Queue;

   protected body Future_Resource is
      function Current_Status return Futures.Status is (Status);

      procedure Set_Status (Value : Futures.Status) is
      begin
         Status := Value;
      end Set_Status;

      entry Wait_Until_Done (Value : out Futures.Status)
        when Current_Status in Futures.Done | Futures.Failed is
      begin
         Value := Status;
      end Wait_Until_Done;

      function Handle_Location return Location_Index is (Location);

      procedure Set_Location (Value : Location_Index) is
      begin
         Location := Value;
      end Set_Location;
   end Future_Resource;

   function Make_Handles return Handle_Array is
   begin
      return Result : Handle_Array do
         for Index in Result'Range loop
            Result (Index) := Index;
         end loop;
      end return;
   end Make_Handles;

   function Make_Futures return Future_Array is
   begin
      return Result : Future_Array do
         for Index in Result'Range loop
            Result (Index).Set_Location (Index);
         end loop;
      end return;
   end Make_Futures;

   protected body Slots is
      entry Acquire (Slot : out Future_Resource_Access)
        when Acquired < Handles'Last is
      begin
         Acquired := Acquired + 1;

         declare
            New_Handle : constant Future_Handle := Handles (Acquired);
         begin
            Slot := Processing_Slots (New_Handle)'Access;
         end;
         pragma Assert (Slot.Handle_Location = Acquired);
      end Acquire;

      procedure Release (Slot : not null Future_Resource_Access) is
         From : constant Location_Index := Slot.Handle_Location;
         To   : constant Location_Index := Acquired;
         pragma Assert (From <= To);
         --  Only a slot that has been acquired can be released

         From_Handle : constant Future_Handle := Handles (From);
         To_Handle   : constant Future_Handle := Handles (To);
      begin
         Handles (From) := To_Handle;
         Handles (To)   := From_Handle;

         --  After having moved the locations of the handles, we need to
         --  update the slots so that they point to correct locations again
         Processing_Slots (From_Handle).Set_Location (To);
         Processing_Slots (To_Handle).Set_Location (From);

         Acquired := Acquired - 1;
      end Release;
   end Slots;

   procedure Shutdown is
   begin
      Queue.Shutdown;
   end Shutdown;

   task body Loader is
      Request : Resource_Read_Request;
      Stop    : Boolean := False;
   begin
      Orka.OS.Set_Task_Name (Name);

      loop
         Queue.Dequeue (Request, Stop);
         exit when Stop;

         begin
            Request.Future.Get.all.Set_Status (Futures.Running);
            declare
               Time_Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

               File  : Byte_Array_File'Class := Open_File (SU.To_String (Request.Path));
               Bytes : Byte_Array_Access := File.Read_File;

               Time_End : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

               use type Ada.Real_Time.Time;
            begin
               Request.Processor
                 (Bytes, Time_End - Time_Start, Request.Path, Job_Queue, Request.Future);
            end;
         exception
            when Error : others =>
               Ada.Text_IO.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
               Request.Future.Get.all.Set_Status (Futures.Failed);
               --  TODO Store exception occurrence?
         end;
      end loop;
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
   end Loader;

end Orka.Resources.Loader;
