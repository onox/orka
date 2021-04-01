# Textures

!!! info
    The various objects described on this page are declared in
    the package `Orka.Rendering.Textures`.

!!! note "TODO"

To verify that the kind and format of the sampler and texture are
compatible, call procedure `Verify_Compatibility`:

```ada linenums="1"
Uniform_1 : Uniform_Sampler := Program_1.Uniform_Sampler ("matrixBuffer");

Uniform_1.Verify_Compatibility (Buffer_Texture_1);
```

## Samplers

!!! note "TODO"
