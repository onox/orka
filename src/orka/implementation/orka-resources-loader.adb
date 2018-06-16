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
with Ada.Real_Time;
with Ada.Text_IO;

with Orka.Containers.Ring_Buffers;
with Orka.OS;

package body Orka.Resources.Loader is

   function Is_Extension (Path, Extension : String) return Boolean is
     (Path (Path'Last - Extension'Length .. Path'Last) = "." & Extension);

   use type Loaders.Loader_Access;

   package Loader_Vectors is new Ada.Containers.Vectors (Positive, Loaders.Loader_Access);
   --  Using a vector instead of a map gives looking up a loader a time
   --  complexity of O(n) instead of O(1), but it is assumed that there
   --  will only be a handful of registered loaders

   protected Resource_Loaders is
      procedure Register (Loader : Loaders.Loader_Ptr);
      --  Add the loader to the list of loaders

      function Has_Loader (Path : String) return Boolean;
      --  Return True if a loader has been registered that can load the
      --  file at the given path based on its extension, False otherwise

      function Loader (Path : String) return Loaders.Loader_Ptr;
      --  Return a loader based on the extension of the given path
   private
      Loaders_Vector : Loader_Vectors.Vector;
   end Resource_Loaders;

   protected body Resource_Loaders is
      procedure Register (Loader : Loaders.Loader_Ptr) is
      begin
         if (for some L of Loaders_Vector => L.Extension = Loader.Extension) then
            raise Constraint_Error with
              "Already registered loader for extension " & Loader.Extension;
         end if;

         Loaders_Vector.Append (Loader);
      end Register;

      function Has_Loader (Path : String) return Boolean is
        (for some Loader of Loaders_Vector => Is_Extension (Path, Loader.Extension));

      function Loader (Path : String) return Loaders.Loader_Ptr is
      begin
         for Loader of Loaders_Vector loop
            if Is_Extension (Path, Loader.Extension) then
               return Loader;
            end if;
         end loop;

         raise Constraint_Error;
      end Loader;
   end Resource_Loaders;

   -----------------------------------------------------------------------------

   type Pair is record
      Location : Locations.Location_Ptr;
      Loader   : Loaders.Loader_Ptr;
   end record;

   package Pair_Vectors is new Ada.Containers.Vectors (Positive, Pair);

   protected Resource_Locations is
      procedure Add (Location : Locations.Location_Ptr; Loader : Loaders.Loader_Ptr);

      function Read_Data
        (Loader : Loaders.Loader_Ptr;
         Path   : String) return not null Byte_Array_Access;
      --  Return the data of the file identified by the given path in the
      --  first of the locations that match with the given loader
      --
      --  If none of the locations contains a file identified by the
      --  given path, the exception Locations.Name_Error is raised.
      --
      --  If the file at the given path could not be read for any reason
      --  then any kind of error is raised.
   private
      Pairs : Pair_Vectors.Vector;
   end Resource_Locations;

   protected body Resource_Locations is

      procedure Add (Location : Locations.Location_Ptr; Loader : Loaders.Loader_Ptr) is
         Element : constant Pair := (Location => Location, Loader => Loader);
      begin
         -- Check that the same combination of location and loader isn't
         -- added multiple times
         if (for some Pair of Pairs => Pair = Element) then
            raise Constraint_Error with "Location already added for the given loader";
         end if;

         Pairs.Append (Element);
      end Add;

      function Read_Data
        (Loader : Loaders.Loader_Ptr;
         Path   : String) return not null Byte_Array_Access
      is
         File_Not_Found : Boolean := False;
         Occurrence : Ada.Exceptions.Exception_Occurrence;
      begin
         for Pair of Pairs loop
            if Loader = Pair.Loader then
               begin
                  return Pair.Location.Read_Data (Path);
               exception
                  --  Catch the exception raised if the location does not
                  --  have the requested resource, because there might be
                  --  multiple locations that we want to try
                  when Error : Locations.Name_Error =>
                     File_Not_Found := True;
                     Ada.Exceptions.Save_Occurrence (Occurrence, Error);
               end;
            end if;
         end loop;

         if File_Not_Found then
            Ada.Exceptions.Reraise_Occurrence (Occurrence);
         end if;

         --  No locations have been added for the given loader
         raise Constraint_Error with "No locations added for the given loader";
      end Read_Data;

   end Resource_Locations;

   -----------------------------------------------------------------------------

   type Read_Request is record
      Path   : SU.Unbounded_String;
      Future : Futures.Pointers.Mutable_Pointer;
      Time   : Ada.Real_Time.Time;
   end record;

   Null_Request : constant Read_Request := (others => <>);

   function Get_Null_Request return Read_Request is (Null_Request);

   package Buffers is new Orka.Containers.Ring_Buffers
     (Read_Request, Get_Null_Request);

   protected Queue is
      entry Enqueue (Element : Read_Request);

      entry Dequeue (Element : out Read_Request; Stop : out Boolean);

      procedure Shutdown;
   private
      Requests    : Buffers.Buffer (Maximum_Requests);
      Should_Stop : Boolean := False;
   end Queue;

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

   -----------------------------------------------------------------------------

   procedure Register (Loader : Loaders.Loader_Ptr) is
   begin
      Resource_Loaders.Register (Loader);
   end Register;

   procedure Add_Location (Location : Locations.Location_Ptr; Loader : Loaders.Loader_Ptr) is
   begin
      Resource_Locations.Add (Location, Loader);
   end Add_Location;

   function Load (Path : String) return Futures.Pointers.Mutable_Pointer is
      Slot : Futures.Future_Access;
   begin
      if not Resource_Loaders.Has_Loader (Path) then
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
            Future  : Futures.Pointers.Reference renames Request.Future.Get;
            Promise : Futures.Promise'Class renames Futures.Promise'Class (Future.Value.all);
         begin
            Promise.Set_Status (Futures.Running);

            declare
               Path : constant String := SU.To_String (Request.Path);
               Loader : Loaders.Loader_Ptr renames Resource_Loaders.Loader (Path);

               use Ada.Real_Time;

               Time_Start : constant Time := Clock;

               Bytes : constant Byte_Array_Access := Resource_Locations.Read_Data (Loader, Path);

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
               Promise.Set_Failed (Error);
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
