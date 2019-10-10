# Logging

## Logging

The package `:::ada Orka.Logging` provides subprograms to print messages
to the screen. This can help with debugging your application.

Each message has a source and a severity:


| Source           | Severity |
|------------------|----------|
| Executor         | Error    |
| Game\_Loop       | Warning  |
| Resource\_Loader | Info     |
| Other            | Debug    |

Currently application should only use `Other`. To print a message
to the screen, call procedure `Insert_Message`:

```ada
Orka.Logging.Insert_Message
  (From       => Other,
   Level      => Info,
   Identifier => 0,
   Message    => "FPS: " & FPS'Image);
```

Identifier should be unique per source and is an application-defined
number. This procedure may be called from any task.

### Formatting

To trim a string, use the function `Trim`.

To format a `:::ada Ada.Real_Time.Time_Span`, use the function `Image`.

!!! example
    The following code:

    ```ada linenums="1"
       use Ada.Real_Time;
       use Orka.Logging;

       Insert_Message
         (From       => Game_Loop,
          Level      => Debug,
          Identifier => 0,
          Message    => "Tick resolution: " & Trim (Image (Tick)));
    ```

    will print:

    `[13:37:00.000042 DEBUG] [GAME_LOOP] 0: Tick resolution: 0.001 us`

## OpenGL debugging

The graphics driver has the ability to notify the application of various
events that may occur. Each message has a source, message type, severity,
implementation-defined ID, and a human-readable description:

| Source           | Type                 | Severity     |
|------------------|----------------------|--------------|
| OpenGL           | Error                | Notification |
| Window\_System   | Deprecated\_Behavior | High         |
| Shader\_Compiler | Undefined\_Behavior  | Medium       |
| Third\_Party     | Portability          | Low          |
| Application      | Performance          |              |
| Other            | Other                |              |
|                  | Marker               |              |
|                  | Push\_Group          |              |
|                  | Pop\_Group           |              |

To make sure the driver generates messages, use the `Debug` flag when
creating the context:

```ada
Initialized : constant Orka.Windows.GLFW.Active_GLFW'Class :=
  Orka.Windows.GLFW.Initialize (Major => 4, Minor => 3, Debug => True);
```

To print all messages currently in the log to the screen, execute:

```ada
Orka.Debug.Flush_Log;
```

Function `:::ada Orka.Debug.Logged_Messages` returns the number of messages
currently in the log.
After the log has been flushed, a default callback can be enabled which
automatically prints new messages to the screen:

```ada
Orka.Debug.Enable_Print_Callback;
```

This procedures enables messages with any severity, including `Low` severity.
To enable your own callback procedure instead of the default, call:

```ada
GL.Debug.Set_Message_Callback (My_Callback'Access);
```

The callback may be called by the graphics driver from multiple tasks,
concurrently, and/or asynchronously after executing an OpenGL command
if `:::ada GL.Toggles.State (Debug_Output_Synchronous) = Disabled`.

!!! example
    ```ada linenums="1"
    declare
       Messages : constant Natural := Orka.Debug.Logged_Messages;
    begin
       Put_Line ("Flushing" & Messages'Image & " messages in the debug log:");
    end;
    Orka.Debug.Flush_Log;

    Orka.Debug.Enable_Print_Callback;
    Put_Line ("Set callback for debug messages");
    ```

### Enabling or disabling specific messages

Messages of a specific source, type, level, or ID can be enabled or
disabled by calling one of the `:::ada GL.Debug.Set` procedures.

### Manually generating messages

!!! note "TODO"

### Object labels

!!! note "TODO"

### Groups

!!! note "TODO"
