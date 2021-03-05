# General-purpose computing on the GPU

Besides rendering, Orka can also be used for general purpose computing
on the GPU (GPGPU) by using compute shaders. With compute shaders,
a framebuffer is not needed; compute shaders can read from and write to any
buffers and/or images (textures) that are binded.

A shader program that
uses a vertex and fragment shader does require a framebuffer.
The default framebuffer is not available unless a window has been created.
If the default framebuffer cannot be used, a new framebuffer object must be
created and binded instead.

## Creating a context

An OpenGL context can be created using either GLFW or EGL. Using GLFW requires
the presence of a windowing system such as Wayland or the windowing system of
Windows. EGL supports multiple platforms and can be used on Wayland as well
as without any windowing system by directly using a GPU device.

A context is created by calling the function `Create_Context` in the package
`:::ada Orka.Windows.GLFW` or `:::ada Orka.Contexts.EGL`. The requested
version of OpenGL must be given and optionally some flags. The following
flags can be used:

- **Debug**. Creates a debug context. The video driver may generate errors,
  warnings, and other useful information. If `Debug` is `False` then the
  video driver may choose to not enable full debug output. In order to
  actually enable log messages, make sure to call `:::ada Orka.Debug.Set_Log_Messages`.
  See [Logging](/logging/#opengl-debugging) for more information.
- **Robust**. Creates a context where out-of-bound writes to buffers are
  discarded and reads return zero. In addition, the GL context can be queried
  for graphics resets that have occurred.
- **No_Error**. Creates a context where any OpenGL API call will not generate
  an error, but instead cause undefined behavior. This might increase performance
  a bit, but should only be enabled after having verified that errors do not
  occur.

### GLFW

When using GLFW, an OpenGL context can be created by creating a (hidden) window:

```ada
Context : constant Orka.Contexts.Context'Class := Orka.Windows.GLFW.Create_Context
  (Version => (4, 2), Flags  => (Debug => True, others => False));

Window : constant Orka.Windows.Window'Class
  := Orka.Windows.GLFW.Create_Window (Context, Width => 1, Height => 1, Visible => False);
pragma Unreferenced (window);
```

When using GLFW, the actual context is created and made current on the calling
task by the `Create_Window` function and depends on the active windowing system.

### EGL

If a windowing system is not available or depending on it is not desired,
EGL must be used instead:

```ada
Context : constant Orka.Contexts.Context'Class := Orka.Contexts.EGL.Create_Context
  (Version => (4, 2), Flags  => (Debug => True, others => False));
pragma Unreferenced (Context);
```

The context is created and made current on the calling task by the `Create_Context`
function.

#### Enumerating devices

The function `Devices` in the package `:::ada EGL.Objects.Devices` can return
a list of devices:

```ada
Devices : constant EGL.Objects.Devices.Device_List := EGL.Objects.Devices.Devices;

Device_1 : EGL.Objects.Devices.Device renames Devices (Devices'First);
```

On Linux, this function will usually return two devices: a device that
represents the video card in the machine and a 'device' to do software rendering
on the CPU.

The name of a device can be retrieved via the function `Name` of the device.
If the name cannot be retrieved, an empty string is returned. A list of device
extensions that a device supports can be obtained via the function `Extensions`:

```ada
for Extension of Device_1.Extensions loop
   Ada.Text_IO.Put_Line (EGL.SU.To_String (Extension));
end loop;
```

If the device has an extension called `EGL_MESA_device_software` then rendering
will happen on the CPU.

#### Creating a context for a device

Package `:::ada Orka.Contexts.EGL` has a second function `Create_Context` that
uses a device as the first parameter to create a context using that particular
device:

```ada
Context : constant Orka.Contexts.Context'Class := Orka.Contexts.EGL.Create_Context
  (Device  => Device_1,
   Version => (4, 2),
   Flags   => (Debug => True, others => False));
pragma Unreferenced (Context);
```

If the parameter `Device` is not given, then the first device in the list
that would be returned by function `Devices` is used.

## Algorithms

### Prefix sum

!!! note "TODO"

### Fast Fourier Transform (FFT)

!!! note "TODO"
