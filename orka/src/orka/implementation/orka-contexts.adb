--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Strings.Unbounded;

with Orka.Strings;

package body Orka.Contexts is

   function Image (Version : Context_Version) return String is
     (Strings.Trim (Version.Major'Image) & "." & Strings.Trim (Version.Minor'Image));

   function Image (Flags : Context_Flags) return String is
      package SU renames Ada.Strings.Unbounded;

      Result : SU.Unbounded_String;
   begin
      if Flags.Debug then
         SU.Append (Result, " debug");
      end if;
      if Flags.Robust then
         SU.Append (Result, " robust");
      end if;
      if Flags.No_Error then
         SU.Append (Result, " no-error");
      end if;
      return Strings.Trim (SU.To_String (Result));
   end Image;

end Orka.Contexts;
