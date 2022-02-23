# Matrices

Matrices are an ordered set of columns and form a linear combination
with the elements of a vector as weights when multiplied with a vector.
In computer graphics they are used to apply transformations to vectors.
While vectors are in ℝ^4^, matrices are in ℝ^4×4^.

## Transformations

Various kinds of transformations exist:

- *Linear transform*. A transform in which vector addition and scalar
  multiplication is preserved.

- *Affine transform*. A transform that includes a linear transform and
  a translation. Parallelism of lines remain unchanged, but lengths
  and angles may not. A concatenation of affine transforms is affine.

- *Orthogonal transform*. A transform whose columns are orthogonal
  or perpendicular to each other.
  The inverse of an orthogonal matrix is equal to its transpose.
  A concatenation of orthogonal matrices is orthogonal.

Affine transforms are: translation, rotation, uniform scaling, and
orthographic projection.
Perspective projection is *not* affine because the parallelism of the lines
is changed.
Rotations are orthogonal transforms.

### Applying transformations

A vector can be transformed by multiplying it with some matrix:

```ada
Vector_B : constant Vector_Type := Matrix_A * Vector_A;
```

The vector `Vector_B` can then be again transformed by yet another matrix.
This creates a chain or sequence of transformations to a vector.
A single matrix can describe the sequence of transformations by concatenating
the transformation matrices using multiplication:

```ada
Matrix_C : constant Matrix_Type := Matrix_B * Matrix_A;
```

!!! tip
    It is more efficient to perform `:::ada A * (B * V)`
    than `:::ada A * B * V` where `V` is a vector.
    The latter will perform one matrix-matrix and one matrix-vector
    multiplication, while the former will perform two matrix-vector
    multiplications.

!!! summary
    Matrix multiplication is associative: `:::ada A * (B * C) = (A * B) * C`.

!!! warning "Matrix multiplication is not commutative"
    Matrix multiplication is not commutative: `:::ada A * B /= B * A`.

### Translation

A vector can be translated by multiplying it with a translation matrix
created with the function `T`:

```ada
Offset       : constant Vector_Type := (1.0, 2.0, 0.0, 1.0);
Moved_Vertex : constant Vector_Type := T (Offset) * Vertex;
```

Alternatively, the `+` operator can be used:

```ada
Moved_Vertex : constant Vector_Type := Offset + Vertex;
```

The inverse of a translation is equal to the negated offset:
T^-1^ (t) = T (-t).

### Rotation

A vector can be rotated around the x-axis with `Rx`, y-axis with `Ry`, z-axis with `Rz`,
or any arbitrary axis with function `R`. The given `Angle` must be in radians.
The returned matrix is orthogonal and affine.

For example:

```ada
Axis  : constant Vector_Type  := (1.0, 2.0, 0.0, 1.0);
Angle : constant Element_Type := 0.5 * Ada.Numerics.Pi;

Rotated_Vertex : constant Vector_Type := R (Axis, Angle) * Vertex;
```

Additional instances of `Rx`, `Ry`, `Rz`, and `R` exist with an
extra parameter `Point` that describe the center of the rotation.
Without this extra parameter, the rotation will happen around the origin.

A rotation matrix is orthogonal and thus the inverse is equal to its transpose:

```ada
Vertex : constant Vertex_Type := Transpose (Rotation_Matrix) * Rotated_Vertex;
```

and equal to a rotation described by the negated angle:
R^-1^ (a, o) = R (a, -o).

#### From A to B

A matrix describing the rotation from a vector A to B can be created
with another instance of `R`:

```ada
Matrix_A_To_B : constant Vector_Type := R (From => Vector_A, To => Vector_B);
```

#### Quaternions

[Quaternions](/transforms/quaternions) describe rotations with 4 numbers
instead of a 16 (like a 4 × 4 matrix).
Unlike the Euler transform, they are not affected by something called 'gimbal lock'.
A quaternion is implemented as a vector and can be converted to a
rotation matrix with another instance of function `R`:

```ada
Matrix_A : constant Matrix_Type := R (Vectors.Vector4 (Quaternion_A));
```

An extra parameter `Point` can be provided to rotate around the given point
instead of the origin.

#### Euler parameters

Given a rotation matrix, the Euler parameters yaw, pitch, and roll can
be extracted using the function `Euler`:

```ada
Yaw_Pitch_Roll : constant Vector_Type := Euler (Rotation_Matrix);
```

with `Rotation_Matrix` begin some rotation matrix. The angles are in radians.
The `W` component is always zero and the yaw is zero if the pitch is +/- 90 degrees.

### Scaling

Vectors can be scaled using the function `S`. The `X`, `Y`, and `Z` components
describe the amount by which a vector is scaled in the corresponding axis.
The `W` component is ignored.

The transformation is affine if all three axes are scaled by the same amount.

Alternatively, the `*` operator can be used for a uniform scaling:

```ada
Factor : constant := 2.0;

Scaled_Vertex : constant Vector_Type  := Factor * Vertex;
```

The inverse of a scaling is equal to the reciprocal of the factors:
S^-1^ (s) = S (1/s~x~, 1/s~y~, 1/s~z~).

## Projections

!!! note "TODO FOV, Perspective, Orthographic"
