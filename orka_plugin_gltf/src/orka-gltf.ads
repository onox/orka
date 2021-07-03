--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

with JSON.Types;

with GL.Types;

private package Orka.glTF is
   pragma Preelaborate;

   package SU renames Ada.Strings.Unbounded;

   Maximum_Name_Length : constant := 64;

   package Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => Maximum_Name_Length);

   function To_String (Value : Name_Strings.Bounded_String) return String
     renames Name_Strings.To_String;

   package Types is new JSON.Types (Long_Integer, GL.Types.Single);

   subtype Natural_Optional is Integer range -1 .. Integer'Last;

   Undefined : constant := Natural_Optional'First;

end Orka.glTF;
