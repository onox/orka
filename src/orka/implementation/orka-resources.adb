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

with Ada.Unchecked_Conversion;

package body Orka.Resources is

   function Convert (Bytes : Byte_Array) return String is
      subtype Bytes_String is String (1 .. Bytes'Length);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Resources.Byte_Array, Target => Bytes_String);
   begin
      return Convert (Bytes);
   end Convert;

   function Convert (Text : String) return Byte_Array is
      subtype Bytes_String is String (1 .. Text'Length);
      subtype Bytes_String_Array is Byte_Array (1 .. Text'Length);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Bytes_String, Target => Bytes_String_Array);
   begin
      return Convert (Text);
   end Convert;

end Orka.Resources;
