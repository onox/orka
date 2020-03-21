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

with Orka.Transforms.SIMD_Vectors;

generic
   with package Vector_Transforms is new Orka.Transforms.SIMD_Vectors (<>);

   type Matrix_Type is array (Index_Homogeneous) of Vector_Transforms.Vector_Type;

   with function Multiply_Matrices (Left, Right : Matrix_Type) return Matrix_Type;
   with function Multiply_Vector
     (Left  : Matrix_Type;
      Right : Vector_Transforms.Vector_Type) return Vector_Transforms.Vector_Type;
   with function Transpose_Matrix (Matrix : Matrix_Type) return Matrix_Type;
package Orka.Transforms.SIMD_Matrices is
   pragma Pure;

   package Vectors renames Vector_Transforms;

   subtype Element_Type is Vectors.Element_Type;
   subtype Vector_Type is Vectors.Vector_Type;

   subtype Matrix4 is Matrix_Type;

   subtype Vector4 is Vector_Type;

   function Identity_Value return Matrix_Type is
     (((1.0, 0.0, 0.0, 0.0),
       (0.0, 1.0, 0.0, 0.0),
       (0.0, 0.0, 1.0, 0.0),
       (0.0, 0.0, 0.0, 1.0)))
   with Inline;
   --  Return the identity matrix

   function Zero_Point return Vector_Type renames Vectors.Zero_Point;
   --  Return a zero vector that indicates a point. The fourth (W) component
   --  is 1.

   --  Linear transform: a transform in which vector addition and scalar
   --  multiplication is preserved.

   --  Affine transform: a transform that includes a linear transform and
   --  a translation. Parallelism of lines remain unchanged, but lengths
   --  and angles may not. A concatenation of affine transforms is affine.
   --
   --  Orthogonal matrix: the inverse of the matrix is equal to the transpose.
   --  A concatenation of orthogonal matrices is orthogonal.

   function T (Offset : Vector_Type) return Matrix_Type;
   --  Translate points by the given amount
   --
   --  Matrix is affine.
   --
   --  The inverse T^-1 (t) = T (-t).

   function Rx (Angle : Element_Type) return Matrix_Type;
   --  Rotate around the x-axis by the given amount in radians
   --
   --  Matrix is orthogonal and affine.
   --
   --  The inverse Rx^-1 (o) = Rx (-o) = (Rx (o))^T.

   function Ry (Angle : Element_Type) return Matrix_Type;
   --  Rotate around the y-axis by the given amount in radians
   --
   --  Matrix is orthogonal and affine.
   --
   --  The inverse Ry^-1 (o) = Ry (-o) = (Ry (o))^T.

   function Rz (Angle : Element_Type) return Matrix_Type;
   --  Rotate around the z-axis by the given amount in radians
   --
   --  Matrix is orthogonal and affine.
   --
   --  The inverse Rz^-1 (o) = Rz (-o) = (Rz (o))^T.

   function R (Axis : Vector_Type; Angle : Element_Type) return Matrix_Type;
   --  Rotate around the given axis by the given amount in radians
   --
   --  Matrix is orthogonal and affine.
   --
   --  The inverse is R^-1 (a, o) = R (a, -o) = (R (a, o))^T.

   function R (Quaternion : Vector_Type) return Matrix_Type;
   --  Converts a quaternion to a rotation matrix
   --
   --  Note: the quaternion must be a unit quaternion (normalized).

   function S (Factors : Vector_Type) return Matrix_Type;
   --  Scale points by the given amount in the x-, y-, and z-axis
   --
   --  If all axes are scaled by the same amount, then the matrix is
   --  affine.
   --
   --  The inverse is S^-1 (s) = S (1/s_x, 1/s_y, 1/s_z).

   function "*" (Left, Right : Matrix_Type) return Matrix_Type renames Multiply_Matrices;

   function "*" (Left : Matrix_Type; Right : Vector_Type) return Vector_Type renames Multiply_Vector;

   function "+" (Offset : Vector_Type; Matrix : Matrix_Type) return Matrix_Type;
   --  Add a translation transformation to the matrix

   function "*" (Factor : Element_Type; Matrix : Matrix_Type) return Matrix_Type;
   --  Add a scale transformation to the matrix

   function Transpose (Matrix : Matrix_Type) return Matrix_Type renames Transpose_Matrix;

   procedure Rotate_At_Origin (Matrix : in out Matrix_Type; Axis : Vector_Type; Angle : Element_Type);
   --  Add a rotation transformation to the matrix with the center
   --  of rotation at the origin to the matrix

   procedure Rotate (Matrix : in out Matrix_Type; Axis : Vector_Type;
                     Angle  : Element_Type; Point : Vector_Type);
   --  Add a rotation transformation to the matrix with the center
   --  of rotation at the given point to the matrix

   procedure Rotate_At_Origin (Matrix : in out Matrix_Type; Quaternion : Vector_Type);
   --  Add a rotation transformation based on a quaternion to the matrix
   --  with the center of rotation at the origin to the matrix
   --
   --  Note: the quaternion must be a unit quaternion (normalized).

   procedure Rotate (Matrix : in out Matrix_Type; Quaternion : Vector_Type;
                     Point  : Vector_Type);
   --  Add a rotation transformation based on a quaternion to the matrix
   --  with the center of rotation at the given point to the matrix
   --
   --  Note: the quaternion must be a unit quaternion (normalized).

   procedure Rotate_X_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type);
   --  Add a rotation transformation around the X axis with the center
   --  of rotation at the origin to the matrix

   procedure Rotate_Y_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type);
   --  Add a rotation transformation around the Y axis with the center
   --  of rotation at the origin to the matrix

   procedure Rotate_Z_At_Origin (Matrix : in out Matrix_Type; Angle : Element_Type);
   --  Add a rotation transformation around the Z axis with the center
   --  of rotation at the origin to the matrix

   procedure Rotate_X (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type);
   --  Add a rotation transformation around the X axis with the center
   --  of rotation at the given point to the matrix

   procedure Rotate_Y (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type);
   --  Add a rotation transformation around the Y axis with the center
   --  of rotation at the given point to the matrix

   procedure Rotate_Z (Matrix : in out Matrix_Type; Angle : Element_Type; Point : Vector_Type);
   --  Add a rotation transformation around the Z axis with the center
   --  of rotation at the given point to the matrix

   --  procedure Rotate_Quaternion (Matrix : in out Matrix_Type; Quaternion : ?);
   --  procedure Rotate_Euler (Matrix : in out Matrix_Type; Euler : ?);

   procedure Translate (Matrix : in out Matrix_Type; Offset : Vector_Type);
   --  Add a translation transformation to the matrix

   procedure Scale (Matrix : in out Matrix_Type; Factors : Vector_Type);
   procedure Scale (Matrix : in out Matrix_Type; Factor : Element_Type);

   procedure Transpose (Matrix : in out Matrix_Type);
   --  Transpose the matrix

   use type Element_Type;

   function FOV (Width, Distance : Element_Type) return Element_Type;
   --  Return an appropriate field of view in radians for a given screen
   --  width and view distance in physical units (mm/inches)
   --
   --  For example, for a 35 mm frame (which is 36 mm wide) and a
   --  50 mm standard lens, the function gives ~ 39 degrees.

   function Finite_Perspective (FOV, Aspect, Z_Near, Z_Far : Element_Type) return Matrix_Type
     with Pre => FOV > 0.0 and Aspect > 0.0 and Z_Near > 0.0 and Z_Far > Z_Near;
   --  Return a matrix providing perspective projection with a depth
   --  range of [0, 1]
   --
   --  The vertical field of view must be in radians.

   function Infinite_Perspective (FOV, Aspect, Z_Near : Element_Type) return Matrix_Type
     with Pre => FOV > 0.0 and Aspect > 0.0 and Z_Near > 0.0;
   --  Return a matrix providing perspective projection with Z_far to
   --  infinite and a depth range of [0, 1]
   --
   --  The vertical field of view must be in radians.

   function Infinite_Perspective_Reversed_Z
     (FOV, Aspect, Z_Near : Element_Type) return Matrix_Type
   with Pre => FOV > 0.0 and Aspect > 0.0 and Z_Near > 0.0;
   --  Return a matrix providing perspective projection with Z_far to
   --  infinite and a depth range of [1, 0]
   --
   --  The vertical field of view must be in radians.

   function Orthographic (X_Mag, Y_Mag, Z_Near, Z_Far : Element_Type) return Matrix_Type
     with Pre => Z_Near >= 0.0 and Z_Far >= 0.0;
   --  Return a matrix providing orthographic projection with a depth
   --  range of [0, 1]

end Orka.Transforms.SIMD_Matrices;
