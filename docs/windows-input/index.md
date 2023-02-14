# Windows and input devices

AWT is used to create windows that can display 3D graphics
and to manage input devices like the pointer, keyboard, and
gamepads. It has a similar purpose as GLFW and SDL, but is fully
written in Ada.

### Initialization

To initialize AWT, create a context using the function `Create_Context`
in package `:::ada Orka.Contexts.AWT`. It will initialize AWT if
needed.

To manually initialize AWT, in case subprograms in various child
packages of `AWT` are to be called before there is a possibility
to create a context, call `AWT.Initialize`.

### Processing window and input events

After AWT has been initialized, either manually or by creating a
rendering context, a window can be created and then events sent
by the compositor can be processed:

```ada
declare
   Interval : constant Duration := To_Duration (Microseconds (16_667));
begin
   while not Window.Should_Close loop
      AWT.Process_Events (Interval);

      --  Query the state of the window or input devices
   end loop;
end;
```

It is not needed to keep the rendering context current in the task
in which the AWT event loop resides. The context can be moved to a
separate render task before entering the event loop.
See [Moving a context](/rendering/contexts/#moving-a-context).
