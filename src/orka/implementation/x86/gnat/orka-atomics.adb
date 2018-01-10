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

package body Orka.Atomics is

   procedure Add_And_Fetch
     (Ptr : access Unsigned_32; Value : Unsigned_32)
   with Import, Convention => Intrinsic, External_Name => "__sync_add_and_fetch_4";

   function Subtract_And_Fetch
     (Ptr : access Unsigned_32; Value : Unsigned_32) return Unsigned_32
   with Import, Convention => Intrinsic, External_Name => "__sync_sub_and_fetch_4";

   procedure Increment (Value : in out Unsigned_32) is
   begin
      Add_And_Fetch (Value'Unrestricted_Access, 1);
   end Increment;

   function Decrement (Value : in out Unsigned_32) return Unsigned_32 is
   begin
      return Subtract_And_Fetch (Value'Unrestricted_Access, 1);
   end Decrement;

   procedure Add (Value : in out Unsigned_32; Addition : Unsigned_32) is
   begin
      Add_And_Fetch (Value'Unrestricted_Access, Addition);
   end Add;

   procedure Sub (Value : in out Unsigned_32; Subtraction : Unsigned_32) is
      Result : Unsigned_32;
      pragma Unreferenced (Result);
   begin
      Result := Subtract_And_Fetch (Value'Unrestricted_Access, Subtraction);
   end Sub;

end Orka.Atomics;
