# Quaternions

A quaternion represents a rotation around some axis and have two
benefits over rotation matrices.

First, a quaternion requires only 4 numbers and thus can be stored
in a vector (ℝ^4^), while a rotation matrix requires 16 numbers (ℝ^4×4^).
A quaternion thus needs much less space to be stored.

Second, quaternions are not affected by singularities (a degree of
freedom is lost when 'gimbal lock' occurs) like Euler angles are,
which are needed to construct a rotation matrix.

The function `Identity` returns a quaternion that does not perform
any rotation.

## Chaining rotations

Multiple rotations can be chained with the `*` operator:

```ada
Rotation_A : constant Quaternion := R ((1.0, 0.0, 0.0, 0.0), To_Radians (30.0));
Rotation_B : constant Quaternion := R ((1.0, 0.0, 0.0, 0.0), To_Radians (60.0));

Rotation_C : constant Quaternion := Rotation_B * Rotation_A;
```

The difference between two rotations is another rotation and can be
obtained with the function `Difference`:
`:::ada Difference (Rotation_A, Rotation_C)` is equivalent to `Rotation_B`.

The functions `Conjugate` and `Inverse` return the conjugate and inverse
of a quaternion.

## Length and normalization

The function `Norm` returns the magnitude of the quaternion.
A unit quaternion has a magnitude equal to 1.
The function `Normalize` will normalize a quaternion and return
a unit quaternion. The function `Normalized` will return `True`
or `False` depending on whether the quaternion is normalized or not.

## Axis and angle

The function `To_Axis_Angle` converts a quaternion to a `Axis_Angle`,
while `From_Axis_Angle` converts the `Axis_Angle` back to a quaternion.
An `Axis_Angle` consists of an `Axis`, a normalized `Direction` vector,
and an `Angle` in radians.

Alternatively, the function `R` can return a quaternion given a
separate `Axis` and `Angle`.

## Rotations of vectors

The function `R` can return a quaternion describing the rotation
from one vector to another.
The function `Rotate` rotates and returns a vector according to a given rotation.

## Interpolation

Function `Slerp` returns the interpolated unit quaternion on the shortest
arc between two normalized quaternions, while `Lerp` returns the
interpolation on the chord between two normalized quaternions.
