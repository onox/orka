with Orka.Logging.Default;
with Orka.Strings;

separate (AWT.Monitors)
procedure Log_Information (Monitor : AWT.Monitors.Monitor'Class) is
   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   procedure Log is new Orka.Logging.Default.Generic_Log (Window_System);

   State : constant AWT.Monitors.Monitor_State := Monitor.State;
begin
   Log (Info,
     (if Monitor.Is_Connected then "Connected" else "Disconnected") &
     " monitor " & (+State.Name) & " (" & Trim (Monitor.ID'Image) & ")");
   Log (Info, "  offset:  " & Trim (State.X'Image) & ", " & Trim (State.Y'Image));
   Log (Info, "  size:    " &
     Trim (State.Width'Image) & Orka.Strings.Unicode (" × ") & Trim (State.Height'Image));
   Log (Info, "  refresh: " & Trim (Image (State.Refresh)));
end Log_Information;
