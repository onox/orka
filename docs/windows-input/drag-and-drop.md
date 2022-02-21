# Drag and drop

To support dragging and dropping objects on a window, a complicated dance
of callbacks and subprogram calls must be performed. Two procedures must be
overridden, `On_Drag` and `On_Drop`.

## Dragging

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

## Dropping

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
      Object.Dnd_Signal.Set;
   end if;
end On_Drop;
```

A data transfer must not be initiated in the `On_Drop` callback, but in the
task containing the event loop. In the example above, an object of the protected
type `Signal` with a procedure `Set` and an entry `Wait` is used to set a signal.

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
      Window.Dnd_Signal.Wait;

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
