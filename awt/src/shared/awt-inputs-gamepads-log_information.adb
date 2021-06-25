separate (AWT.Inputs.Gamepads)
procedure Log_Information (Gamepad : AWT.Inputs.Gamepads.Gamepad'Class) is
   Connection   : constant Connection_Kind := Gamepad.Connection;
   Is_Connected : constant Boolean := Connection /= Disconnected;
begin
   Messages.Log (Info,
     (if Is_Connected then "Connected" else "Disconnected") & " " & Name (Gamepad));
   Messages.Log (Info, "  serial:     " & Gamepad.Serial_Number);
   Messages.Log (Info, "  GUID:       " & String (Gamepad.GUID));
   Messages.Log (Info, "  connection: " & Connection'Image);
end Log_Information;
