--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Types;
with GL.Debug;

private package GL.Debug_Types is
   pragma Preelaborate;

   use GL.Types;
   use GL.Debug;

   type Source_Array is array (Size range <>) of Source;
   type Type_Array is array (Size range <>) of Message_Type;
   type Severity_Array is array (Size range <>) of Severity;

   type Source_Array_Access is access all Source_Array;
   type Type_Array_Access is access all Type_Array;
   type Severity_Array_Access is access all Severity_Array;

   type UInt_Array_Access is access all Orka.Unsigned_32_Array;
   type Size_Array_Access is access all Size_Array;

   type String_Access is access String;

end GL.Debug_Types;
