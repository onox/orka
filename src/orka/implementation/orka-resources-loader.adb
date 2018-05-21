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

with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;

with Orka.OS;

package body Orka.Resources.Loader is

   function Is_Extension (Path, Extension : String) return Boolean is
     (Path (Path'Last - Extension'Length .. Path'Last) = "." & Extension);

   use type Loaders.Loader_Access;

   package Loader_Vectors is new Ada.Containers.Vectors (Positive, Loaders.Loader_Access);
   --  Using a vector instead of a map gives looking up a loader a time
   --  complexity of O(n) instead of O(1), but it is assumed that there
   --  will only be a handful of registered loaders

   protected Extensions is
      procedure Register (Loader : Loaders.Loader_Ptr);
      --  Add the loader to the list of loaders

      function Has_Loader (Path : String) return Boolean;
      --  Return True if a loader has been registered that can load the
      --  file at the given path based on its extension, False otherwise

      function Loader (Path : String) return Loaders.Loader_Ptr;
      --  Return a loader based on the extension of the given path
   private
      Extension_Loaders : Loader_Vectors.Vector;
   end Extensions;

   protected body Extensions is
      procedure Register (Loader : Loaders.Loader_Ptr) is
      begin
         if (for some L of Extension_Loaders => L.Extension = Loader.Extension) then
            raise Constraint_Error with
              "Already registered loader for extension " & Loader.Extension;
         end if;

         Extension_Loaders.Append (Loader);
      end Register;

      function Has_Loader (Path : String) return Boolean is
        ((for some Loader of Extension_Loaders => Is_Extension (Path, Loader.Extension)));

      function Loader (Path : String) return Loaders.Loader_Ptr is
      begin
         for Loader of Extension_Loaders loop
            if Is_Extension (Path, Loader.Extension) then
               return Loader;
            end if;
         end loop;

         raise Constraint_Error;
      end Loader;
   end Extensions;

   procedure Register (Loader : Loaders.Loader_Ptr) is
   begin
      Extensions.Register (Loader);
   end Register;

   function Load (Path : String) return Futures.Pointers.Mutable_Pointer is
      Slot : Futures.Future_Access;
   begin
      if not Extensions.Has_Loader (Path) then
         raise Resource_Load_Error with "No registered loader for " & Path;
      end if;

      Queues.Slots.Manager.Acquire (Slot);
      declare
         Pointer : Futures.Pointers.Mutable_Pointer;
      begin
         Pointer.Set (Slot, Queues.Release_Future'Unrestricted_Access);
         Queue.Enqueue
           ((Path   => SU.To_Unbounded_String (Path),
             Future => Pointer,
             Time   => Ada.Real_Time.Clock));
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
            Path : constant String := SU.To_String (Request.Path);

            Future : Futures.Pointers.Reference renames Request.Future.Get;
            Loader : Loaders.Loader_Ptr renames Extensions.Loader (Path);
         begin
            Future.Set_Status (Futures.Running);

            declare
               use Ada.Real_Time;

               Time_Start : constant Time := Clock;

               File  : Byte_Array_File'Class := Open_File (Path);
               Bytes : Byte_Array_Access := File.Read_File;

               Time_End : constant Time := Clock;

               Reading_Time : constant Time_Span := Time_End - Time_Start;

               procedure Enqueue (Element : Jobs.Job_Ptr) is
               begin
                  Job_Queue.Enqueue (Element, Request.Future);
               end Enqueue;
            begin
               Loader.Load ((Bytes, Reading_Time, Request.Time, Request.Path), Enqueue'Access);
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
