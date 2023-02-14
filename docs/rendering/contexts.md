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

An OpenGL context can be created with either AWT or EGL.
Using AWT requires the presence of a windowing system such
as Wayland.
EGL supports multiple platforms and can be used on Wayland as well
as without any windowing system by directly using a GPU device.

A context is created by calling the function `Create_Context` in the packages
`:::ada Orka.Contexts.AWT` or `:::ada Orka.Contexts.EGL`.
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

!!! warning "AWT is currently not available on Windows"
    Currently, AWT has a backend for Wayland only. If you are on Windows, there
    are several options:

    1. Use [WSLg][url-wslg] with the d3d12 Gallium driver from Mesa.
       See [Mesamatrix][url-mesamatrix] for a list of extensions supported
       by the d3d12 driver.

    2. Write a backend for AWT using the win32ada Alire crate for Windows.
       If and once it has been made open source under the Apache-2.0 license,
       you could use code from [aglw][url-aglw].

    3. Create a context and window using SDL through the [sdlada][url-sdlada] crate.

    4. Use a surfaceless context with EGL, assuming you can install and link
       with the EGL library from the `mingw-w64-x86_64-mesa` package on msys2.
       The mentioned package should be installed automatically by Alire as a
       dependency of the Alire crate orka\_egl.

### AWT

When using AWT, an OpenGL context must be created first:

```ada
Context : constant Orka.Contexts.Surface_Context'Class := Orka.Contexts.AWT.Create_Context
  (Version => (4, 2),
   Flags   => (Debug | Robust => True, others => False));

Window : aliased Orka.Windows.Window'Class := Orka.Contexts.AWT.Create_Window
  (Context, Width => 1280, Height => 720);
```

The context is created and made current on the calling task by the `Create_Context`
function.

!!! tip
    Call function `Framebuffer_Resized` in the rendering task to detect
    that the default framebuffer must be recreated before rendering to it.

### EGL

If a windowing system is not available or depending on it is not desired,
EGL can be used directly instead:

```ada
Context : constant Orka.Contexts.Context'Class := Orka.Contexts.EGL.Create_Context
  (Version => (4, 2),
   Flags   => (Debug => True, others => False));
```

This will create a context using the 'device' platform of EGL.
The context is created and made current on the calling task by the `Create_Context`
function.

!!! note
    A context that uses the Wayland platform can be created using the function
    `Create_Context` in package `:::ada Orka.Contexts.EGL.Wayland`.
    The Wayland platform is also used when creating a context on Linux via AWT.

!!! tip "Listing supported EGL extensions"
    The Alire crate orka\_egl provides the executable orka\_egl\_info, which will print
    the supported client and platforms extensions.

#### Enumerating devices

The function `Devices` in the package `:::ada EGL.Objects.Devices` can return
a list of devices:

```ada
Devices : constant EGL.Objects.Devices.Device_List := EGL.Objects.Devices.Devices;

Device_1 : EGL.Objects.Devices.Device renames Devices (Devices'First);
```

On Linux, this function will usually return two devices: a device that
represents the video card in the machine and a 'device' that does software
rendering on the CPU.

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
```

If the parameter `Device` is not given, then the first device in the list
that would be returned by function `Devices` is used.

!!! summary
    An EGL context can be created using different platforms:

    - The Wayland platform, created using `:::ada Orka.Contexts.EGL.Wayland`,
      allows you to render things on the screen in a window, but rendering
      is done using the same GPU as the one used by the Wayland compositor.

    - The 'device' platform, created using `:::ada Orka.Contexts.EGL`,
      allows you to render off-screen using any device returned by
      `:::ada EGL.Objects.Devices.Devices`, but the results cannot be
      displayed in a window.

## Moving a context

A context is automatically made current on the calling task after
it is created, but it can be moved to another task if necessary.
To do so, the context must first be made non-current in the first task
and the first task must then subsequently signal to the second task that
it can make the context current by performing a rendezvous:

```ada
Context.Move_To (Render_Task, Window);
```

The second task must implement the task interface `Task_With_Surface_Context`:

```ada
task Render_Task is new Orka.Contexts.Task_With_Surface_Context with
   entry Move_Context
     (Context : not null Orka.Contexts.Surface_Context_Access;
      Window  : in out Orka.Windows.Window'Class);
end Render_Task;
```

This  task can then make the context current after accepting the `Move_Context`
call:

```ada
accept Move_Context
  (Context : not null Orka.Contexts.Surface_Context_Access;
   Window  : in out Orka.Windows.Window'Class)
do
   Context.Make_Current (Window);
end Move_Context;
```

It is recommended that the task on which the context is current calls
`:::ada Context.Make_Not_Current` after completing its execution or when
some exception is raised. To do this, the task can declare a variable
containing a pointer to a context:

```ada
Context : Orka.Contexts.Surface_Context_Access;
```

And then set it inside the `accept` block:

```ada
Render_Task.Context := Context;
```

!!! tip "Moving a surfaceless context"
    A surfaceless context implementing the interface `Context` can be
    moved to a task implementing the task interface `Task_With_Context`.
    It must implement the following entry:

    ```ada
    entry Move_Context (Context : not null Orka.Contexts.Context_Access;
    ```

  [url-aglw]: https://github.com/ohenley/aglw
  [url-wslg]: https://github.com/microsoft/wslg
  [url-mesamatrix]: https://mesamatrix.net/
  [url-sdlada]: https://alire.ada.dev/crates/sdlada
