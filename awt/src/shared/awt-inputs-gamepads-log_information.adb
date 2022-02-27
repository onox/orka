separate (AWT.Inputs.Gamepads)
procedure Log_Information (Gamepad : AWT.Inputs.Gamepads.Gamepad) is
   Connection   : constant Connection_Kind := Gamepad.Connection;
   Is_Connected : constant Boolean := Connection /= Disconnected;
begin
   Log (Info,
     (if Is_Connected then "Connected" else "Disconnected") & " " & Gamepad.Name);
   Log (Info, "  serial:     " & Gamepad.Serial_Number);
   Log (Info, "  GUID:       " & String (Gamepad.GUID));
   Log (Info, "  connection: " & Connection'Image);
end Log_Information;
