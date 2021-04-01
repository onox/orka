# Contexts

To render 3D graphics with OpenGL in a window or to perform general-purpose
computing on the GPU, a context needs to be created first. A context must
be current on exactly one task and only rendering commands from that task
are considered valid commands for that particular context.

In Orka, a context is automatically made current on the calling task after
it is created, but the context can be moved to another task by first making
it non-current in the first task and then making it current in the other task.

Depending on the platform, a context can be attached to a window when it is
made current. After attaching the context to a window, a default framebuffer
will be available for drawing commands. Any drawing commands that draw to
the default framebuffer will render to the surface of the window to which
the context has been attached.

## Creating a context

An OpenGL context can be created using either AWT or GLFW, or EGL.
Using AWT or GLFW requires the presence of a windowing system such
as Wayland or the windowing system of Windows.
EGL supports multiple platforms and can be used on Wayland as well
as without any windowing system by directly using a GPU device.

A context is created by calling the function `Create_Context` in the packages
`:::ada Orka.Windows.AWT`, `:::ada Orka.Windows.GLFW` or `:::ada Orka.Contexts.EGL`.
The requested version of OpenGL must be given and optionally some flags.
The following flags can be used:

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

### AWT

When using AWT, an OpenGL context must be created first:

```ada linenums="1"
Context : constant Orka.Contexts.Surface_Context'Class := Orka.Contexts.AWT.Create_Context
  (Version => (4, 2),
   Flags   => (Debug | Robust => True, others => False));
```

The context is created and made current on the calling task by the `Create_Context`
function.

!!! note "TODO" Create window and render task
!!! note "TODO" Multiple windows

### GLFW

When using GLFW, an OpenGL context can be created by creating a window:

```ada linenums="1"
Context : constant Orka.Contexts.Surface_Context'Class := Orka.Windows.GLFW.Create_Context
  (Version => (4, 2),
   Flags   => (Debug => True, others => False));

Window : constant Orka.Windows.Window'Class
  := Orka.Windows.GLFW.Create_Window (Context, Width => 1280, Height => 720);
```

When using GLFW, the actual context is created and made current on the calling
task by the `Create_Window` function and depends on the active windowing system.

### EGL

If a windowing system is not available or depending on it is not desired,
EGL can be used directly instead:

```ada linenums="1"
Context : constant Orka.Contexts.Context'Class := Orka.Contexts.EGL.Create_Context
  (Version => (4, 2),
   Flags   => (Debug => True, others => False));
```

This will create a context using the 'device' platform of EGL. A context that uses
the Wayland platform can be created using the function `Create_Context` in package
`:::ada Orka.Contexts.EGL.Wayland`. The Wayland platform is also used when creating
a context on Linux via AWT.

The context is created and made current on the calling task by the `Create_Context`
function.

#### Enumerating devices

The function `Devices` in the package `:::ada EGL.Objects.Devices` can return
a list of devices:

```ada linenums="1"
Devices : constant EGL.Objects.Devices.Device_List := EGL.Objects.Devices.Devices;

Device_1 : EGL.Objects.Devices.Device renames Devices (Devices'First);
```

On Linux, this function will usually return two devices: a device that
represents the video card in the machine and a 'device' to do software rendering
on the CPU.

The name of a device can be retrieved via the function `Name` of the device.
If the name cannot be retrieved, an empty string is returned. A list of device
extensions that a device supports can be obtained via the function `Extensions`:

```ada linenums="1"
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

```ada linenums="1"
Context : constant Orka.Contexts.Context'Class := Orka.Contexts.EGL.Create_Context
  (Device  => Device_1,
   Version => (4, 2),
   Flags   => (Debug => True, others => False));
```

If the parameter `Device` is not given, then the first device in the list
that would be returned by function `Devices` is used.
