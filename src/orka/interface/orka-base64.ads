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

with Ada.Streams;

private package Orka.Base64 is
   pragma Preelaborate;

   use Ada.Streams;

   Data_Prefix : constant String;

   function Base64_Encoded (Text : String) return Boolean is
     (Text'Length >= Data_Prefix'Length
        and then Text (Text'First .. Data_Prefix'Length) = Data_Prefix)
     with Inline;

   function Decode (Input : String) return Stream_Element_Array;

   Encoding_Error : exception;

private

   Data_Prefix : constant String := "data:text/plain;base64,";

end Orka.Base64;
