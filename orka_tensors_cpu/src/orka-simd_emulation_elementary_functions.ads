--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Ada.Numerics;

generic
   type Vector_Index_Type is (<>);
   type Element_Type is digits <>;
   type Vector_Type is array (Vector_Index_Type) of Element_Type;
package Orka.SIMD_Emulation_Elementary_Functions is
   pragma Preelaborate;

   function Sqrt (Value : Element_Type) return Element_Type with Inline;

   function Is_Valid (Value : Element_Type) return Boolean is (Value'Valid) with Inline;

   function "**" (Left, Right : Vector_Type) return Vector_Type with Inline;
   --  0.0 ** X = 1.0 if X = 0.0
   --  0.0 ** X = 0.0 if X > 0.0

   function Exp (Elements : Vector_Type) return Vector_Type with Inline;
   function Log (Elements : Vector_Type) return Vector_Type with Inline;
   function Log10 (Elements : Vector_Type) return Vector_Type with Inline;
   function Log2 (Elements : Vector_Type) return Vector_Type with Inline;

   function Sin (Elements : Vector_Type) return Vector_Type with Inline;
   function Cos (Elements : Vector_Type) return Vector_Type with Inline;
   function Tan (Elements : Vector_Type) return Vector_Type with Inline;

   function Arcsin (Elements : Vector_Type) return Vector_Type with Inline;
   function Arccos (Elements : Vector_Type) return Vector_Type with Inline;
   function Arctan (Left, Right : Vector_Type) return Vector_Type with Inline;

   function Radians_To_Degrees return Element_Type is (180.0 / Ada.Numerics.Pi) with Inline;
   function Degrees_To_Radians return Element_Type is (Ada.Numerics.Pi / 180.0) with Inline;

end Orka.SIMD_Emulation_Elementary_Functions;
