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

with GL.Types;

package Orka.glTF.Accessors is
   pragma Preelaborate;

   subtype Stride_Natural is Natural range Natural'First .. 255;

   type Component_Kind is (Byte, Unsigned_Byte, Short, Unsigned_Short, Unsigned_Int, Float);

   type Attribute_Kind is (Scalar, Vector2, Vector3, Vector4, Matrix2, Matrix3, Matrix4);

   Attributes_Length : constant array (Attribute_Kind) of Positive :=
     (Scalar  => 1,
      Vector2 => 2,
      Vector3 => 3,
      Vector4 => 4,
      Matrix2 => 4,
      Matrix3 => 9,
      Matrix4 => 16);

   type Double_Array is array (Positive range <>) of GL.Types.Double;

   type Accessor (Elements_Count : Positive) is record
      View_ID    : SU.Unbounded_String;
      Offset     : Natural;
      Stride     : Stride_Natural;
      Component  : Component_Kind;
      Normalized : Boolean := False;
      Count      : Positive;
      Kind       : Attribute_Kind;
      Min, Max   : Double_Array (1 .. Elements_Count);
   end record;
--     with Dynamic_Predicate => Accessor.Elements_Count = Attributes_Length (Accessor.Kind);

end Orka.glTF.Accessors;
