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

with Interfaces.C;

package Orka is
   pragma Pure;

   type Index_Homogeneous is (X, Y, Z, W);

   subtype Index_2D is Index_Homogeneous range X .. Y;
   subtype Index_3D is Index_Homogeneous range X .. Z;

   type Integer_16 is new Interfaces.C.short;

   type Unsigned_32 is mod 2 ** 32
     with Size => 32;

   type Float_32 is new Interfaces.C.C_float;
   type Float_64 is new Interfaces.C.double;

   type Time is private;

   function "-" (Left, Right : Time) return Duration;
   function "-" (Left : Time; Right : Duration) return Time;

private

   type Time is new Duration;

   function "-" (Left, Right : Time) return Duration is (Duration (Left) - Duration (Right));

   function "-" (Left : Time; Right : Duration) return Time is (Time (Duration (Left) - Right));

end Orka;
