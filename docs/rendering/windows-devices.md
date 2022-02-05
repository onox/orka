# Windows and input devices

AWT is used to create windows that can display 3D graphics with
an OpenGL context and to manage input devices like the pointer, keyboard, and
gamepads. It has a similar purpose as GLFW and SDL.

## Windows

Given a `:::ada Surface_Context'Class` context created by the function `Create_Context`
in package `:::ada Orka.Contexts.AWT` (see [Contexts](/rendering/contexts/#awt)), a
window can be created with the function `Create_Window`:

```ada
Window : aliased Orka.Windows.Window'Class := Orka.Contexts.AWT.Create_Window
  (Context, Width => 1280, Height => 720);
```

If the type `AWT_Window` is extended, then the function `Create_Window` must be overridden:

```ada
overriding
function Create_Window
 (Context            : aliased Orka.Contexts.Surface_Context'Class;
  Width, Height      : Positive;
  Title              : String  := "";
  Samples            : Natural := 0;
  Visible, Resizable : Boolean := True;
  Transparent        : Boolean := False) return My_Window is
begin
  return Result : constant My_Window :=
    (Orka.Contexts.AWT.Create_Window
      (Context, Width, Height, Title, Samples,
       Visible     => Visible,
       Resizable   => Resizable,
       Transparent => Transparent) with others => <>);
end Create_Window;
```

The function `Framebuffer_Resized` can be used to recreate the default
framebuffer before rendering to it:

```ada
procedure Render (Object : in out My_Window) is
begin
   if Object.Framebuffer_Resized then
      Object.Create_And_Use_New_Default_Framebuffer;
   end if;

   Object.FB.Clear ((Color => True, others => False));
   Object.Render_To_Default_Framebuffer;

   Object.Swap_Buffers;
end Render;
```

### Querying state

The window state can be retrieved using the overloaded function `State`:

```ada
Window_State : constant AWT.Windows.Window_State := Window.State;
```

`Window_State` contains booleans like `Visible`, `Resizable`, `Decorated`,
and `Transparent`.

The size of the window can be retrieved using `Width` and `Height`.
The optional margin around the window with `Margin`. A non-zero margin
can be used to render a shadow or client-side decoration around the window.

The framebuffer state can be retrieved with another instance of the
function `State`:

```ada
Framebuffer_State : constant AWT.Windows.Framebuffer_State := Window.State;
```

It contains the `Width` and `Height` of the framebuffer as well as the `Scale`.
The `Scale` is a `Positive` and is usually equal to 1.

### Changing state

!!! note "TODO Set app ID, title, margin, size, size limits, size mode, visibility, pointer"

### Closing

The function `Should_Close` can be used to detect that a window should be
closed and that it is no longer needed to process any events or do any
rendering. The program should then exit any loops in the main and rendering tasks.
For example, a rendering task could contain the following code:

```ada
while not Window.Should_Close loop
   Window.Render;
end loop;

Context.Make_Not_Current;
```

To programmatically close the window, call procedure `Close`. After having
called this procedure, the function `Should_Close` will return `True`.

It may also happen that the user will try to close the window through the
windowing system. For example, by pressing a button in the decoration or
by pressing ++alt+f4++. This request can be intercepted by overriding the
function `On_Close`:

```ada
overriding
function On_Close (Object : Test_Window) return Boolean is
begin
   Messages.Log (Debug, "User tried to close the window");
   return True;
end On_Close;
```

This function should return `True` if the window should actually be closed,
and `False` otherwise. This is useful to display a dialog window on the
screen that asks the user for confirmation. If the user confirms, then
the application should call the procedure `Close`.

The default implementation of `On_Close` always returns `True`.

## Drag and drop

To support dragging and droppping objects on a window, a complicated dance
of callbacks and subprogram calls must be performed. Two procedures must be
overridden, `On_Drag` and `On_Drop`.

### Dragging

The first one is invoked when the user drag something over the window. Call
procedure `Set_Action` in `AWT.Drag_And_Drop` in response to this event:

```ada
overriding
procedure On_Drag
  (Object : in out Test_Window;
   X, Y   : AWT.Inputs.Fixed)
is
   use all type AWT.Inputs.Action_Kind;
   use type AWT.Inputs.Fixed;
begin
   AWT.Drag_And_Drop.Set_Action (if X < 100.0 and Y < 100.0 then Copy else None);
end On_Drag;
```

The parameters `X` and `Y` indicate to where the user is trying to drag
something. The procedure `Set_Action` in package `:::ada AWT.Drag_And_Drop`
must be called to set the requested action that must happen when the user
drops something on the current focused window.
Valid values are `Copy`, `Move`, `Ask`, or `None`.

### Dropping

Procedure `On_Drop` is invoked when the user finally drops something on
the window. The function `Valid_Action` must be used to determine the
final action that must occur. If the value is not equal to `None`, a
data transfer can be initiated or the application may ask the user
to make a choice between some actions.

```ada
overriding
procedure On_Drop
  (Object : in out Test_Window)
is
   use all type AWT.Inputs.Action_Kind;

   Action : constant AWT.Inputs.Action_Kind := AWT.Drag_And_Drop.Valid_Action;
begin
   if Action /= None then
      Dnd_Signal.Set;
   end if;
end On_Drop;
```

A data transfer must not be initiated in the `On_Drop` callback, but in the
task containing the event loop. In the example above, a simple protected object
`Dnd_Signal` with a procedure `Set` and an entry `Wait` is used to set a signal.

In the event loop, wait for the signal and then perform the drag-and-drop by
calling subprogram `Get` in package `:::ada AWT.Drag_And_Drop`.
This can be a function that blocks until the data transfer has been completed,
or it can be a procedure with a callback to a protected procedure with one
parameter of the type `:::ada AWT.SU.Unbounded_String`. In both cases, the actual
data transfer may take place in some background task.

After the data transfer has been completed, call procedure `Finish` to either
accept or reject the drag-and-drop operation.
To accept the operation, use `Copy` or `Move`. If the last set action was `Ask`,
then the value may either be `Copy` or `Move`, otherwise it must match the
last set action. To reject the operation, use `None`.

```ada
while not Window.Should_Close loop
   AWT.Process_Events (Interval);

   select
      Dnd_Signal.Wait;

      declare
         Result : constant String := AWT.Drag_And_Drop.Get;
      begin
         Orka.OS.Put_Line ("value: '" & Orka.Strings.Strip_Line_Term (Result) & "'");
         AWT.Drag_And_Drop.Finish (AWT.Inputs.Copy);
      end;
   else
      null;
   end select;
end loop;
```

## Clipboard

!!! note "TODO"

## Pointers and keyboards

!!! note "TODO Function `State` for `Pointer_State` and `Keyboard_State`"

## Gamepads

AWT also supports gamepads:

- **Mappings**. Use mappings from the [SDL gamecontroller database][url-sdl-gamecontroller-db].

- **Events**. Listen for (dis)connection events.

- **Force-feedback**. Play and cancel rumble and periodic force-feedback effects.

- **Motion sensor**. Get the linear acceleration and angular velocity using the motion
  sensor of a gamepad.

- **Battery**. Retrieve the capacity and charging state of the battery of a gamepad.

- **LED**. Get and set the color of the LED of a gamepad.

!!! note "TODO"

## Monitors

!!! note "TODO"

  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
