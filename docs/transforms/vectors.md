# Vectors

Vectors are an ordered array of numbers in ℝ. All vectors have
four components `X`, `Y`, `Z`, and `W` and are thus in ℝ^4^,
but often the `W` component is equal to zero or one. Thus
they usually they represent a point or direction in a 3-D space.

## Points and directions

A `Point` is a vector with the `W` component equal to one
and a `Direction` is a vector with the `W` component equal to zero.

## Arithmetic

The following binary operators can be used on vectors: `+`, `-`, and `*`.
The `*` can also operate on a vector and a single element.
Unary operators that operate on a single vector are: `-` and `abs`.

!!! summary
    Vector addition and multiplication is associative and commutative:

    - `:::ada A + (B + C) = (A + B) + C`

    - `:::ada A + B = B + A`

    Similar for (element-wise) multiplication.

### Points and directions

Subtracting two `Point`s results in a `Direction`.
Adding a `Direction` to a `Point` gives another `Point`.

Subtracing or adding two `Direction`s gives another `Direction`.
A `Direction` can also be scaled by a single element.

A `Point` cannot be scaled by an arbitrary element, but can be mirrored
using the unary `-` operator.

## Dot and cross products

The function `Dot` returns the dot or inner product of two vectors.
It is the sum of the product of the corresponding elements in the vectors.
It can also be expressed in terms of the norm (magnitude) and angle:

`:::ada Dot (A, B) = Norm (A) * Norm (B) * Angle (A, B)`

The dot product of two unit vectors represents the angle between them
and is 0 if they are orthogonal, 1 if they are in the same direction,
and -1 if they are in the opposite direction.

!!! info "The dot product **a** ∙ **b** is sometimes written as **a**^T^ **b**"

The cross product of two vectors can be computed with the function `Cross`.

!!! summary
    Dot product is commutative: `:::ada Dot (A, B) = Dot (B, A)`.

    The cross product is anti-commutative: `::ada Cross (A, B) = Cross (-B, A)`.

## Length and normalization

The functions `Magnitude` and `Norm` return a scalar that represents
the magnitude of a vector; the distance between a point in a space and
the origin.

A unit vector is a vector with a magnitude of one. The function `Normalized`
will return `True` if a vector is a unit vector and `False` if the magnitude
is not equal to one.
The functions `Normalize` and `Normalize_Fast` will normalize the given vector.
Thus the magnitude of the vector returned by these functions is one. `Normalize_Fast`
may use SIMD intrinsics for single precision floating-point vectors, which will
be faster, but also less accurate than the implementation used by `Normalize`.

## Distance and angle

The function `Distance` returns the distance between two `Point`s and the
function `Angle` returns the angle in radians between two vectors.
The angle between two unit vectors is equal to the dot product of the two vectors.

## Projection

The function `Projection` returns the projection of a vector on some other vector.
The magnitude of the projected vector is equal to `:::ada Dot (U, V) / Magnitude2 (V)`

The function `Perpendicular` returns a vector perpendicular to the projection
of the vector on the second vector.
For example, if `:::ada W = Projection (U, V)`, then `:::ada U - W = Perpendicular (U, V)`
and `:::ada (U - W) + W = U`.
It can also be said that the dot product of the vectors `:::ada U - W` and `W` is zero
and thus they are orthogonal.

## Interpolation

The function `Slerp` returns the interpolated unit vector on the shortest arc
between two vectors. This can be useful for animations.
