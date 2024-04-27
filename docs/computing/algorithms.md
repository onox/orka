# Algorithms

Orka provides a few algorithms which can be used for general purposes.

!!! info
    The various objects described on this page are declared in
    the packages `:::ada Orka.Algorithms` and its child packages.

## Prefix sum

The package `:::ada Orka.Algorithms.Prefix_Sums` provides means to compute
the exclusive parallel prefix sum of a sequence of unsigned integers.

A `Prefix_Sum` object can be created with the function
`Create_Prefix_Sum`:

```ada
PS : Prefix_Sum := Create_Prefix_Sum (Context, Length => Count);
```

`Count` contains a `Positive` number representing the number of elements
for which to compute a prefix sum. The value must be a multiple of 4.
The length which was given to the function `Create_Prefix_Sum` can be queried
by the function `Length` of the created `Prefix_Sum` object.

After a `Prefix_Sum` object has created, it can be used to compute the prefix
sum of a [buffer](/rendering/buffers) by calling the procedure `Compute_Prefix_Sum`:

```ada
PS.Compute_Prefix_Sum (Buffer_1);
```

The length of the given buffer must be equal to the value returned by the
function `Length` of the prefix sum object.

The content of the given buffer is modified to contain the compute prefix sum.
For example, if the data of `Buffer_1` is the following before calling the procedure:

`[0 1 0 0 1 1 0 1 0 0 1 1 1 0 1 0]`

then the data has changed to the following after the procedure returns:

`[0 0 1 1 1 2 3 3 4 4 4 5 6 7 7 8]`

If the original content of the buffer is still needed after computing
the prefix sum, then it should be copied to another before before computing
the prefix sum.

In the example above, the original content may reflect the positions
of elements in some other buffer for which some predicate is true.
The original content and the modified content (the prefix sum) of `Buffer_1`
can then be used to compact the elements of that other buffer in another
compute shader.
This is technique is applied when using a boolean tensor as the index of
some other tensor as shown in
[Using a boolean tensor](/numerics/tensors/indexing/#using-a-boolean-tensor).

!!! summary
    - The length of the buffer must be a multiple of 4.

    - Procedure `Compute_Prefix_Sum` modifies the data of the buffer.

## Fast Fourier Transform (FFT)

The package `:::ada Orka.Algorithms.FFT` provides the type `FFT`,
which can be used to compute the Cooley-Tukey Fast Fourier Transform of a sequence of
floating-point numbers.

First create the `FFT` object using the function `Create_FFT`:

```ada
FFT_1 : FFT := Create_FFT (Context);
```

The Fast Fourier Transform of the data of a buffer can then be computed
with the procedure `Compute_FFT`:

```ada
FFT_1.Compute_FFT (Buffer_1, Width, Height, Transpose => False, Inverse => False);
```

The content of the given buffer is modified to contain the fourier transform.
The length of the given buffer should be two times the product of the given width and height.

If `Transpose` is true then the height must be a power of two, otherwise the width
must be a power of two. If `Inverse` is true then the inverse of the FFT is computed.
