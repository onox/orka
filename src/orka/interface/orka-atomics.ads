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

package Orka.Atomics is
   pragma Preelaborate;

   type Unsigned_32 is mod 2 ** 32;

   procedure Increment (Value : in out Unsigned_32)
     with Inline_Always;
   --  Atomically increment the value by 1

   function Decrement (Value : in out Unsigned_32) return Unsigned_32
     with Inline_Always;
   --  Atomically decrement the value by 1 and return the new value

   procedure Add (Value : in out Unsigned_32; Addition : Unsigned_32)
     with Inline_Always;

   procedure Sub (Value : in out Unsigned_32; Subtraction : Unsigned_32)
     with Inline_Always;

end Orka.Atomics;
