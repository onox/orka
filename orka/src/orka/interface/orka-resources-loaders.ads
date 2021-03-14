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

with Ada.Real_Time;

with Orka.Jobs;
with Orka.Resources.Locations;

package Orka.Resources.Loaders is

   type Resource_Data is record
      Bytes        : Byte_Array_Pointers.Pointer;
      Reading_Time : Ada.Real_Time.Time_Span;
      Start_Time   : Ada.Real_Time.Time;
      Path         : SU.Unbounded_String;
   end record;

   subtype Extension_String is String
     with Dynamic_Predicate => Extension_String'Length <= 4;

   type Loader is limited interface;

   function Extension (Object : Loader) return Extension_String is abstract;
   --  Return the extension of files that the loader can load

   procedure Load
     (Object   : Loader;
      Data     : Resource_Data;
      Enqueue  : not null access procedure (Element : Jobs.Job_Ptr);
      Location : Locations.Location_Ptr) is abstract;
   --  Load the given resource data

   type Loader_Access is access Loader'Class;

   subtype Loader_Ptr is not null Loader_Access;

end Orka.Resources.Loaders;
