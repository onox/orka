# Statistics

The minimum and maximum element in a tensor can be retrieved with the
functions `Min` and `Max`.

The function `Quantile` returns the element when the cumulative distribution
function of the tensor reaches the given probability `P`.
`Median` returns the middle element (when sorted), which is equal to
calling `Quantile` with `:::ada P = 0.5`.

The mean and variance can be computed using the functions `Mean` and `Variance`.
An optional parameter `Offset` can be used to return the unbiased sample variance
instead. The returned value is unbiased if `:::ada Offset = 1` and biased
if `:::ada Offset = 0`.

The standard deviation (the square root of the variance) is returned by
`Standard_Deviation`. Like `Variance`, it has an optional parameter `Offset`.
The returned value is always biased because of the square root, even
when `:::ada Offset = 1`.

## Distributions

The generic package `Generic_Random` provides functions that return a
tensor with one of the following statistical distributions:

- `Uniform`. Values are uniformly distributed in the range 0 .. 1.

- `Normal`. Values are from the standard normal distribution with
  mean 0.0 and variance 1.0. To create a tensor with the distribution
  N(3.0, 2.0) (mean is 3.0 and standard deviation is 2.0), use:

      ```ada
      3.0 + Normal (Shape) * 2.0
      ```

- `Binomial` with parameters `N` and `P`. Returns a tensor where each
  element is the number of successful runs (each value is in 0 .. `N`)
  with each run having a probability of success `P`. Parameter `P`
  must be in 0.0 .. 1.0.

      For example, if we want to perform some experiment 10 times with a
      success probability of 0.1 (10 %) each, and want to know the probability
      that all 10 experiments fail, create a large tensor with a binomial
      distribution:

      ```ada
      Trials : constant := 20_000;

      Tensor : constant CPU_Tensor := Random.Binomial ((1 => Trials), N => 10, P => 0.1);
      Result : constant Element    := CPU_Tensor'(1.0 and (Tensor = 0.0)).Sum / Element (Trials);
      ```

      This gives a `Result` of roughly 0.35 or 35 %.

!!! warning "Keep parameter `N` small for large tensors"
    The runtime cost of the implementation of `Binomial` might depend on
    `N`, thus this number should not be too large for very large tensors.

- `Geometric` with parameter `P`. Create a tensor with a geometric
  distribution, modeling the number of failures. Parameter `P` must be in
  0.0 .. 1.0.

- `Exponential` with parameter `Lambda`.

- `Pareto` with parameters `Xm` and `Alpha`.

- `Laplace` with parameters `Mean` and `B`.

- `Rayleigh` with parameter `Sigma`.

- `Weibull` with parameters `K` and `Lambda`.

- `Poisson` with parameter `Lambda`.

- `Gamma` with parameters `K` and `Theta`.

- `Beta` with parameters `Alpha` and `Beta`.

The package can be instantiated using a type derived from type `Tensor`,
for example:

```ada
use Orka.Numerics.Singles.Tensors;
use Orka.Numerics.Singles.Tensors.CPU;

package Random is new Generic_Random (CPU_Tensor);
```

The type `CPU_Tensor` in package `SIMD_CPU` uses the [xoshiro128++][url-xoshiro]
pseudo-random number generator and needs to be seeded once with a
Duration value before using any of the functions in the generic package:

```ada
Reset_Random (Orka.OS.Monotonic_Clock);
```

  [url-xoshiro]: https://prng.di.unimi.it/
