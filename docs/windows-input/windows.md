# Windows

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

## State

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

## Changing the state

!!! note "TODO Set app ID, title, margin, size, size limits, size mode, visibility, pointer"

## Closing windows

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
