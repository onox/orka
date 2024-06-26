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

with Orka.Transforms.SIMD_Vectors;

generic
   with package Vectors is new Orka.Transforms.SIMD_Vectors (<>);
package Orka.Transforms.SIMD_Quaternions is
   pragma Pure;

   type Quaternion is new Vectors.Vector_Type;

   subtype Vector4 is Vectors.Vector_Type;

   type Axis_Angle is record
      Axis  : Vectors.Direction;
      Angle : Vectors.Element_Type;
   end record
     with Dynamic_Predicate => Vectors.Normalized (Vector4 (Axis));

   Identity : constant Quaternion := [0.0, 0.0, 0.0, 1.0];

   function "+" (Left, Right : Quaternion) return Quaternion;

   function "*" (Left, Right : Quaternion) return Quaternion;

   function "*" (Left : Vectors.Element_Type; Right : Quaternion) return Quaternion;

   function "*" (Left : Quaternion; Right : Vectors.Element_Type) return Quaternion;

   function Conjugate (Elements : Quaternion) return Quaternion;

   function Inverse (Elements : Quaternion) return Quaternion;

   function Norm (Elements : Quaternion) return Vectors.Element_Type;

   function Normalize (Elements : Quaternion) return Quaternion
     with Inline;

   function Normalized (Elements : Quaternion) return Boolean
     with Inline;

   function To_Axis_Angle (Elements : Quaternion) return Axis_Angle
     with Pre => Normalized (Elements);

   function From_Axis_Angle (Value : Axis_Angle) return Quaternion
     with Post => Normalized (From_Axis_Angle'Result);

   function R
     (Axis  : Vector4;
      Angle : Vectors.Element_Type) return Quaternion
   with Pre  => Vectors.Normalized (Axis),
        Post => Normalized (R'Result);
   --  Return a quaternion that will cause a rotation of Angle radians
   --  about the given Axis

   function R (Left, Right : Vector4) return Quaternion
     with Post => Normalized (R'Result);
   --  Return the rotation from direction Left to Right

   function Difference (Left, Right : Quaternion) return Quaternion
     with Post => Normalized (Difference'Result);
   --  Return a quaternion describing the rotation from quaternion Left
   --  to Right (Right is a composite rotation of Left and the result)

   function Rotate
     (Vector   : Vector4;
      Rotation : Quaternion) return Vector4
   with Pre => Normalized (Rotation);

   function Lerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   with Pre  => Normalized (Left) and Normalized (Right) and Time in 0.0 .. 1.0,
        Post => Normalized (Lerp'Result);
   --  Return the interpolated normalized quaternion on the chord
   --  between the Left and Right quaternions.

   function Slerp
     (Left, Right : Quaternion;
      Time        : Vectors.Element_Type) return Quaternion
   with Pre  => Normalized (Left) and Normalized (Right) and Time in 0.0 .. 1.0,
        Post => Normalized (Slerp'Result);
   --  Return the interpolated unit quaternion on the shortest arc
   --  between the Left and Right quaternions.

end Orka.Transforms.SIMD_Quaternions;
