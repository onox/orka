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

   function Is_Extension (Path, Extension : String) return Boolean is
     (Path (Path'Last - Extension'Length .. Path'Last) = "." & Extension);

   function Load (Path : String) return Futures.Pointers.Mutable_Pointer is
      function Loader return Load_Ptr is
      begin
         if Is_Extension (Path, "gltf") then
            return Orka.Resources.Models.glTF.Load'Access;
         elsif Is_Extension (Path, "ktx") then
            return Orka.Resources.Textures.KTX.Load'Access;
         else
            raise Resource_Load_Error with Path & " has an unknown extension";
         end if;
      end Loader;

      Slot : Queues.Slots.Future_Object_Access;
   begin
      Queues.Slots.Manager.Acquire (Slot);
      declare
         Pointer : Futures.Pointers.Mutable_Pointer;
      begin
         Pointer.Set (Futures.Future_Access (Slot), Queues.Release_Future'Unrestricted_Access);
         Queue.Enqueue
           ((Path   => SU.To_Unbounded_String (Path),
             Load   => Loader,
             Future => Pointer));
         return Pointer;
         --  Return Pointer instead of Pointer.Get to prevent
         --  adjust/finalize raising a Storage_Error
      end;
   end Load;

   function Load (Path : String) return Futures.Pointers.Reference is
     (Load (Path).Get);
   --  A helper function to avoid raising a Storage_Error

   protected body Queue is
      entry Enqueue (Element : Read_Request) when not Requests.Full is
      begin
         Requests.Add_Last (Element);
      end Enqueue;

      entry Dequeue (Element : out Read_Request; Stop : out Boolean)
        when Should_Stop or else not Requests.Empty is
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

   procedure Shutdown is
   begin
      Queue.Shutdown;
   end Shutdown;

   task body Loader is
      Name : String renames Task_Name;

      Request : Read_Request;
      Stop    : Boolean := False;
   begin
      Orka.OS.Set_Task_Name (Name);

      loop
         Queue.Dequeue (Request, Stop);
         exit when Stop;

         declare
            Future : Futures.Pointers.Reference renames Request.Future.Get;
         begin
            Future.Set_Status (Futures.Running);

            declare
               Time_Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

               File  : Byte_Array_File'Class := Open_File (SU.To_String (Request.Path));
               Bytes : Byte_Array_Access := File.Read_File;

               Time_End : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

               use type Ada.Real_Time.Time;

               procedure Enqueue (Element : Jobs.Job_Ptr) is
               begin
                  Job_Queue.Enqueue (Element, Request.Future);
               end Enqueue;
            begin
               Request.Load (Bytes, Time_End - Time_Start, Request.Path, Enqueue'Access);
            end;
         exception
            when Error : others =>
               Ada.Text_IO.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
               Future.Set_Status (Futures.Failed);
               --  TODO Store exception occurrence?
         end;

         --  Finalize the smart pointer (Request.Future) to reduce the
         --  number of references to the Future object
         Request := Null_Request;
      end loop;
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Name & ": " & Ada.Exceptions.Exception_Information (Error));
   end Loader;

end Orka.Resources.Loader;
