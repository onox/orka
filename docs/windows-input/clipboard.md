# Clipboard

To copy some text to the clipboard, use procedure `Set` in
package `:::ada AWT.Clipboard`. For example, the following code will
copy the text "copied text" when the user presses ++ctrl+c++:

```ada
declare
   Keyboard : constant AWT.Inputs.Keyboard_State := Window.State;
   use all type AWT.Inputs.Keyboard_Button;
begin
   if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_C) then
      AWT.Clipboard.Set ("copied text");
   end if;
end;
```

To read content from the clipboard, use function `Get` to synchronously
read the content, or the procedure `Get` with the parameter `Callback`
set to point to a protected procedure receiving a value of type
`AWT.SU.Unbounded_String`.
The procedure `Get` will asynchronously read the content from the clipboard
and returns immediately. The callback will be called at any time, during or
after the procedure has returned.

For example, the following code will copy some text from the clipboard
when the user presses ++ctrl+v++:

```ada
if Keyboard.Modifiers.Ctrl and Keyboard.Pressed (Key_V) then
   Orka.OS.Put_Line ("Copied '" & AWT.Clipboard.Get & "' from the clipboard");
end if;
```
