with Orka.Logging;

separate (AWT.Monitors)
procedure Log_Information (Monitor : AWT.Monitors.Monitor'Class) is
   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   State : constant AWT.Monitors.Monitor_State := Monitor.State;
begin
   Messages.Log (Info,
     (if Monitor.Is_Connected then "Connected" else "Disconnected") &
     " monitor " & (+State.Name) & " (" & Trim (Monitor.ID'Image) & ")");
   Messages.Log (Info, "  offset:  " & Trim (State.X'Image) & ", " & Trim (State.Y'Image));
   Messages.Log (Info, "  size:    " &
     Trim (State.Width'Image) & " × " & Trim (State.Height'Image));
   Messages.Log (Info, "  refresh: " & Trim (Image (State.Refresh)));
end Log_Information;
