# Filtering

## Sigma-point Kalman filters

Some implementations of the nonlinear Kalman filters using sigma-points[^1]
(SPKF) exist:

- Unscented Kalman Filters (UKF), additive noise case

- Square-Root Central Difference Kalman Filters (SR-CDKF), additive noise case

Both implementations assume that noise can simply be added to the state
covariance matrix. The square-root form of CDKF should provide more
numerical stability than the regular form of the UKF, but involves more
complicated mathematics.

First, the generic package `Kalman` must be instantiated:

```ada
use Orka.Numerics.Doubles.Tensors;
use Orka.Numerics.Doubles.Tensors.CPU;
use type Orka.Numerics.Doubles.Tensors.Element;

package Tensors renames Orka.Numerics.Doubles.Tensors;
package Kalman is new Orka.Numerics.Kalman (Tensors, CPU_Tensor);
```

### Functions

The sigma-point based Kalman filters require a transition function `F`
that converts a sigma point, and a measurement function `H` to convert
a point to measurement space, so that it can be compared with a measurement.

In linear Kalman filters, `F` and `H` are matrices, but in SPKF, they can
be arbitrary functions. It is of course still possible to multiply a sigma
point with some predefined matrix:

```ada
DT  : constant Duration := 1.0;
DTE : constant Element  := Element (DT);

TF : constant Kalman.Matrix := To_Tensor
  ([1.0, DTE, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, DTE,
    0.0, 0.0, 0.0, 1.0],
   Shape => (4, 4));

function F (Point : Kalman.Vector; DT : Orka.Float_64) return Kalman.Vector is
  (TF * Point);

function H (Point : Kalman.Vector) return Kalman.Vector is
  (To_Tensor ([Element'(Point (1)), Element'(Point (3))]));
```

### Noise matrices

The filter requires two matrices, `Q`, the process noise covariance matrix,
and `R`, the measurement noise covariance matrix. These two matrices
should be diagonal or block diagonal matrices and
determine how much weight is given to the prediction or measurement.
For example, if you have a very noisy sensor, the variances on the main
diagonal of `R` will be quite large, and the filter will give more weight
to the predicted state, partly throwing away the measurements.

```ada
Std_Dev : constant := 0.3;

Q : constant Kalman.Matrix := To_Tensor
  ([0.005, 0.01, 0.0,   0.0,
    0.01,  0.02, 0.0,   0.0,
    0.0,   0.0,  0.005, 0.01,
    0.0,   0.0,  0.01,  0.02],
   Shape => (4, 4));

R : constant Kalman.Matrix := Diagonal ((Std_Dev**2, Std_Dev**2));
```

### Creating a filter

Next, instantiate the generic package `UKF` or `CDKF` and use
the function `Create_Filter` in the instantiated package to
create a filter.

To store a `Filter` in a record, the implementation kind and
dimension of the state and measurement must be specified:

```ada
type Estimator is tagged record
   Filter : Kalman.Filter
     (Kind        => Kalman.Filter_CDKF,
      Dimension_X => 4,
      Dimension_Z => 2);
end record;
```

The `Filter` must still be initialized using the function `Create_Filter`
from one of the instantiated packages.

#### UKF

First instantiate the generic package `UKF`:

```ada
package Kalman_UKF is new Kalman.UKF;
```

And then create the `Filter`:

```ada
Filter : Kalman.Filter := Kalman_UKF.Create_Filter
  (Q       => Q,
   R       => R,
   Weights => Kalman_UKF.Weights (N => 4, A => 0.1, B => 2.0, K => 1.0));
```

`A` must be between 0 and 1, and B and K must be greater than or equal to 0.

`A` should be small to avoid non-local effects. `B` is used to reduce
higher-order errors. For a Gaussian, use `B` = 2 and `K` = 3 - `N`.

#### SR-CDKF

First instantiate the generic package `CDKF`:

```ada
package Kalman_CDKF is new Kalman.CDKF;
```

And then create the `Filter`:

```ada
Filter : Kalman.Filter := Kalman_CDKF.Create_Filter
  (Q       => Q + 16.0 * Identity (4),
   R       => R,
   Weights => Kalman_CDKF.Weights (N => 4, H2 => 3.0));
```

`H2` must be greater than or equal to 1.

The parameter `H2` is the square of the interval size and should be chosen
such that it is equal to the kurtosis of the state's distribution. The
kurtosis is equal to the excess kurtosis + 3. For a Gaussian (normal
distribution), the excess kurtosis is 0, so `H2` should be 3.

!!! warning "Noise matrices must be positive definite"
    The function `Create_Filter` in package `CDKF` will compute the Cholesky
    decomposition for each of the two noise matrices.
    If a matrix is not positive definite, the `Not_Positive_Definite_Matrix`
    exception is raised.

    In the example above a multiple of the identity matrix is added to `Q`
    to make it positive definite.

### Predict and update

After the filter has been created, the procedure `Predict_Update` can be
called to predict a new state (prior) using the transition function `F` and
the old state (posterior).
The prior is subsequently converted to measurement space using function `H`.
Using the process and measurement noise matrices, a new state (new posterior)
between the prior and the measurement is then computed.

```ada
for I in 1 .. Elements.Rows loop
   Kalman_CDKF.Predict_Update (Filter, F'Access, H'Access, DT, Elements (I));
   Filtered_Elements.Set (I, H (Filter.State));
end loop;
```

The current state can be retrieved using the function `State`. If desired,
it can be converted to measurement space by reusing the function `H`.
In the example above, `State` returns a tensor with 4 elements (position and
velocity of x and y), and `H` returns a tensor with 2 elements (position of
x and y).

*[SPKF]: Sigma-Point Kalman Filter
*[CDKF]: Central Difference Kalman Filter
*[UKF]: Unscented Kalman Filter

[^1]:
    "Sigma-Point Kalman Filters for Probabilistic Inference in
    Dynamic State-Space Models", van der Merwe R.,
    Oregon Health & Science University, 2004
