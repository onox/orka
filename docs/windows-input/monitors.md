# Monitors

An array of connected monitors can be obtained using the function
`Monitors` in package `:::ada AWT.Monitors`. Each `Monitor` object
can be identified using the function `ID`. The function `Is_Connected`
will return `True` or `False` depending on whether it is still connected
to the system.

## State

The state of the monitor can be retrieved with the function `State`:

```ada
State : constant AWT.Monitors.Monitor_State := Monitor.State;
```

An object of the type `Monitor_State` contains the following information:

- `X` and `Y` offset of the monitor in some global space.

- `Width` and `Height` of the monitor in physical units.

- `Refresh` interval as a `Duration`.

- `Scale` factor of the output, a `Positive` number.

- `Name` of the monitor. The function `+` can be used to convert
  its value to a `String`.

The function `Log_Information` can be used to log all this information
to the default logger.

## Connection events

Because a user can connect and disconnect monitors at any time while the
application is running, the application may want to listen for (dis)connection
events as they occur.
Extend the type `Monitor_Event_Listener` and override the
procedures `On_Connect` and `On_Disconnect`:

```ada
type Event_Listener is new AWT.Monitors.Monitor_Event_Listener with null record;

overriding procedure On_Connect    (Object : Event_Listener; Monitor : AWT.Monitors.Monitor_Ptr);
overriding procedure On_Disconnect (Object : Event_Listener; Monitor : AWT.Monitors.Monitor_Ptr);
```

and implement the overridden procedures:

```ada
overriding
procedure On_Connect
  (Object  : Event_Listener;
   Monitor : AWT.Monitors.Monitor_Ptr) is
begin
   Monitor.Log_Information;
end On_Connect;

overriding
procedure On_Disconnect
  (Object  : Event_Listener;
   Monitor : AWT.Monitors.Monitor_Ptr) is
begin
   Monitor.Log_Information;
end On_Disconnect;
```

and then create an object of the type to start listening for events:

```ada
Monitor_Listener : Event_Listener;
```

As long as the object exists, the overridden procedures will be called when
a monitor is connected or disconnected.

## Moving a window

If a window is moved to or from a monitor, the procedure `On_Move` is
invoked on the corresponding `Window` object.
If not overriden, then by default information about the monitor will
be printed to the default logger.

```ada
overriding
procedure On_Move
  (Object   : in out My_Window;
   Monitor  : AWT.Monitors.Monitor'Class;
   Presence : AWT.Windows.Monitor_Presence);
```

The parameter `Presence` has the value `Entered` or `Left`, depending
on whether the window was moved to or from the monitor.
