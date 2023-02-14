# Removed features

Orka does not support all features of graphics APIs like OpenGL.
Some functionality in OpenGL does not exist in Vulkan or SPIR-V,
or should not be used because nowadays there are better ways to
do certain things.

## Vulkan

For commonality with a future Vulkan backend, several features have been
removed in the OpenGL backend:

- **Atomic counter buffers**.
  Atomic counters are implemented using SSBO atomic operations in most
  hardware. Furthermore, `:::glsl atomic_uint` is not supported in SPIR-V.
  Use function `:::glsl atomicAdd()` in shaders instead.

- **Shader subroutines**.
    Shader subroutines only worked well on certain hardware/driver
    combinations, while causing recompilations on others like Mesa. In
    addition, each program needed to carry along a lot of extra data to make
    them easy to use. SPIR-V doesn't seem to support subroutines.

    Instead of using shader subroutines, use a switch statement with a
    uniform, or use program pipelines to bind stages of different shader
    programs.

- **Line loop and triangle fan primitive topologies**.
  Some primitive topologies like line loop and triangle fan are not
  (always) supported by Vulkan.
  See [section 20.1 Primitive Topologies][url-vulkan-spec-primitive-topologies]
  and [VkPrimitiveTopology][url-vkprimitivetopology] of the Vulkan specification.

- **8-bit unsigned integers for indices for indexed draws**.
  Indices in a buffer must be 16-bit or 32-bit unsigned integers.
  See [VkIndexType][url-vkindextype].

- **Arbitrary border colors**.
  Only transparent, black, or white texture border colors are supported.
  See [section 16.3.3 Texel Replacement][url-vulkans-spec-border-colors]
  of the Vulkan specification.

- **Non-seamless cube maps**.
  Only seamless cube maps are supported.

Some conventions have changed to Vulkan's:

- **First vertex is the provoking vertex**.
  The first vertex is used as the provoking vertex for vertex output
  attributes. It is used in flat shading where all vertices of a primitive
  have the value of the provoking vertex.

- **Depth of the clipping volume has range [0, 1]**.
  The range of the depth of the clipping volume has been changed from
  the default of [-1, 1] to Vulkan's [0, 1].

- **Origin of window coordinates is upper-left**.
    The origin of the window coordinates has been changed to upper-left,
    which is the origin used by other rendering APIs and windowing systems.

    If a shader uses `:::glsl gl_FragCoord`, then you may need to redefine it:

    ```glsl
    layout(origin_upper_left) in vec4 gl_FragCoord;
    ```

    If the output on the screen is upside down, then render to a texture
    instead and read it in some resolve or tonemapping pass that outputs
    to the default framebuffer.

## Other removed features

Over the years, OpenGL has accumulated a lot of functionality. Some of which
should no longer be used in new applications. This list helps the user
to determine which features do not exist on purpose and how they can be
replaced.

- **Conditional rendering**.
    Conditional rendering in OpenGL requires submitting draw calls and has
    latency if you want to read back the results of the queries.

    There are better ways to do occlusion culling:

    * On the GPU in a compute shader and use indirect drawing

    * On the CPU with a software rasterizer[^1] if you need the results on the
      CPU with low latency

- **Non-indirect multi draw rendering commands**.
  Use the indirect multi draw commands instead.

- **Most setters/getters for pixel (un)packing**.
  Instead properly format your data in the first place. Ada's `:::ada Bit_Order`
  and `:::ada Scalar_Storage_Order` aspects may be used to control endianness.
  Row alignment can still be set because some formats like KTX require
  data to be (not) packed.

- **Reading pixels from the framebuffer**.
  Read pixels from a texture attached to the framebuffer instead.

- **Renderbuffers**.
  Renderbuffers are useless because textures support multisampling via
  [ARB_texture_multisample][url-gl-ext-multisample] since OpenGL 3.2.
  They provide no performance benefit in practice and removing them
  simplifies applications.

- **Sampler state in textures**.
  Sampler objects are used to store sampler state. This is more how the
  hardware and other graphics APIs operate.

- **Shader binaries**.
  Loading program binaries will not be supported. In the future support
  for SPIR-V may be added instead.

- **Transform feedback**.
  Transform feedback is a [legacy feature][url-blog-transform-feedback].
  With transform feedback, primitives are ordered across the draw call
  instead of per-pixel. Use compute shaders with SSBOs or images instead.

- **Uniform arrays**.
  Set separate vectors and matrices, or use [UBOs][url-docs-ubos].

- **Vertex array objects**.
  Vertices can be [pulled programmatically][url-docs-vertex-formats] in
  vertex shaders. This avoids having to track and pass around extra objects
  for vertex formats.

- **Wide lines**.
  Wide lines have hardware-dependent limits and will raise an
  `:::ada Invalid_Value_Error` in a forward-compatible context.
  To render wide lines in a portable way, generate geometry or use a
  geometry shader.

[^1]:
    "Masked Software Occlusion Culling", Hasselgren J., Andersson M.,
    Akenine-Möller T., High Performance Graphics, 2016.

*[SSBO]: Shader Storage Buffer Object
*[SPIR-V]: Standard Portable Intermediate Representation

  [url-docs-ubos]: /rendering/buffers/#ubo
  [url-docs-vertex-formats]: /rendering/vertex-formats/
  [url-blog-transform-feedback]: http://jason-blog.jlekstrand.net/2018/10/transform-feedback-is-terrible-so-why.html
  [url-gl-ext-multisample]: https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_multisample.txt
  [url-vulkan-spec-primitive-topologies]: https://www.khronos.org/registry/vulkan/specs/1.2/html/chap20.html#drawing-primitive-topologies
  [url-vkprimitivetopology]: https://www.khronos.org/registry/vulkan/specs/1.2-extensions/man/html/VkPrimitiveTopology.html
  [url-vkindextype]: https://www.khronos.org/registry/vulkan/specs/1.2/html/chap20.html#VkIndexType
  [url-vulkans-spec-border-colors]: https://www.khronos.org/registry/vulkan/specs/1.2/html/chap16.html#textures-texel-replacement
