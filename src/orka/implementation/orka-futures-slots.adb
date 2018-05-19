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

package body Orka.Futures.Slots is

   protected body Future_Object is
      function Current_Status return Futures.Status is (Status);

      procedure Set_Status (Value : Futures.Status) is
      begin
         case Status is
            when Waiting | Running =>
               Status := Value;
            when Failed | Done =>
               pragma Assert (Value /= Done);
         end case;
      end Set_Status;

      entry Wait_Until_Done (Value : out Futures.Status)
        when Status in Done | Failed is
      begin
         Value := Status;
      end Wait_Until_Done;

      function Handle_Location return Location_Index is (Location);

      procedure Reset_And_Set_Location (Value : Location_Index) is
      begin
         Location := Value;
         Status   := Futures.Waiting;
      end Reset_And_Set_Location;

      procedure Set_Location (Value : Location_Index) is
      begin
         Location := Value;
      end Set_Location;
   end Future_Object;

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
            Result (Index).Reset_And_Set_Location (Index);
         end loop;
      end return;
   end Make_Futures;

   protected body Manager is
      entry Acquire (Slot : out Future_Access) when Acquired < Handles'Length is
      begin
         Acquired := Acquired + 1;

         declare
            New_Handle : constant Future_Handle := Handles (Acquired);
         begin
            Slot := Future_Slots (New_Handle)'Access;
         end;
         pragma Assert (Future_Object_Access (Slot).Handle_Location = Acquired);
      end Acquire;

      procedure Release (Slot : not null Future_Access) is
         From : constant Location_Index := Future_Object_Access (Slot).Handle_Location;
         To   : constant Location_Index := Acquired;
         pragma Assert (From <= To);
         --  Only a slot that has been acquired can be released

         From_Handle : constant Future_Handle := Handles (From);
         To_Handle   : constant Future_Handle := Handles (To);
      begin
         Handles (From) := To_Handle;
         Handles (To)   := From_Handle;

         pragma Assert (Future_Slots (From_Handle)'Access = Slot);

         --  After having moved the locations of the handles, we need to
         --  update the slots so that they point to correct locations again
         Future_Slots (From_Handle).Reset_And_Set_Location (To);
         Future_Slots (To_Handle).Set_Location (From);

         Acquired := Acquired - 1;
      end Release;

      procedure Shutdown is
      begin
         Should_Stop := True;
      end Shutdown;

      function Acquired_Slots return Natural is (Acquired);

      function Stopping return Boolean is (Should_Stop);
   end Manager;

end Orka.Futures.Slots;
