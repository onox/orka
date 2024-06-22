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

with Ada.Numerics.Generic_Elementary_Functions;

package body Orka.SIMD_Emulation_Elementary_Functions is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Element_Type);

   function Sqrt (Value : Element_Type) return Element_Type is (EF.Sqrt (Value));

   function "**" (Left, Right : Vector_Type) return Vector_Type is
      function Power (Left, Right : Element_Type) return Element_Type is
        (if Left = 0.0 and Right = 0.0 then 1.0 else EF."**" (Left, Right));
      --  0.0 ** X = 1.0 if X = 0.0 (EF."**" raises Argument_Error instead of returning 1.0)
      --  0.0 ** X = 0.0 if X > 0.0
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := Power (Left (Offset), Right (Offset));
         end loop;
      end return;
   end "**";

   function Exp (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Exp (Elements (Offset));
         end loop;
      end return;
   end Exp;

   function Log (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Log (Elements (Offset));
         end loop;
      end return;
   end Log;

   function Log10 (Elements : Vector_Type) return Vector_Type is
      Log10 : constant := 2.302585092994046;
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Log (Elements (Offset)) / Log10;
         end loop;
      end return;
   end Log10;

   function Log2 (Elements : Vector_Type) return Vector_Type is
      Log2 : constant := 0.6931471805599453;
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Log (Elements (Offset)) / Log2;
         end loop;
      end return;
   end Log2;

   function Sin (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Sin (Elements (Offset));
         end loop;
      end return;
   end Sin;

   function Cos (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Cos (Elements (Offset));
         end loop;
      end return;
   end Cos;

   function Tan (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Tan (Elements (Offset));
         end loop;
      end return;
   end Tan;

   function Arcsin (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Arcsin (Elements (Offset));
         end loop;
      end return;
   end Arcsin;

   function Arccos (Elements : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Arccos (Elements (Offset));
         end loop;
      end return;
   end Arccos;

   function Arctan (Left, Right : Vector_Type) return Vector_Type is
   begin
      return Result : Vector_Type do
         for Offset in Vector_Type'Range loop
            Result (Offset) := EF.Arctan (Left (Offset), Right (Offset));
         end loop;
      end return;
   end Arctan;

end Orka.SIMD_Emulation_Elementary_Functions;
