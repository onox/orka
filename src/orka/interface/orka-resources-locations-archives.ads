--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

private with DCF.Streams;
private with DCF.Zip;

package Orka.Resources.Locations.Archives is

   type Archive_Location is limited new Location with private;

   overriding
   function Exists (Object : Archive_Location; Path : String) return Boolean;

   overriding
   function Read_Data
     (Object : Archive_Location;
      Path   : String) return Byte_Array_Pointers.Pointer;

   function Create_Location (Path : String) return Location_Ptr;
   --  Return a location that is represented by a document container file
   --  (a Zip-based archive format standardized in ISO/IEC 21320-1:2015)

private

   type Archive_Location is limited new Location with record
      Stream  : aliased DCF.Streams.File_Zipstream;
      Archive : DCF.Zip.Zip_Info;
   end record;

end Orka.Resources.Locations.Archives;
