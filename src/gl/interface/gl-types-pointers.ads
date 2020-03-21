--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2014 Felix Krause <contact@flyx.org>
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

with Interfaces.C.Pointers;

package GL.Types.Pointers is
   pragma Preelaborate;

   --  Pointer types (for use with data transfer functions)

   package Byte_Pointers is new Interfaces.C.Pointers
     (Size, Byte, Byte_Array, Byte'Last);

   package Short_Pointers is new Interfaces.C.Pointers
     (Size, Short, Short_Array, Short'Last);

   package Int_Pointers is new Interfaces.C.Pointers
     (Size, Int, Int_Array, Int'Last);

   package UByte_Pointers is new Interfaces.C.Pointers
     (Size, UByte, UByte_Array, UByte'Last);

   package UShort_Pointers is new Interfaces.C.Pointers
     (Size, UShort, UShort_Array, UShort'Last);

   package UInt_Pointers is new Interfaces.C.Pointers
     (Size, UInt, UInt_Array, UInt'Last);

   package Half_Pointers is new Interfaces.C.Pointers
     (Size, Half, Half_Array, 0);

   package Single_Pointers is new Interfaces.C.Pointers
     (Size, Single, Single_Array, 0.0);

   package Double_Pointers is new Interfaces.C.Pointers
     (Size, Double, Double_Array, 0.0);

   type String_Access is not null access constant String;

   type String_Array is array (Positive range <>) of String_Access;

   type UByte_Array_Access is access UByte_Array;

end GL.Types.Pointers;
