# Logging

The packages `:::ada Orka.Logging` and `:::ada Orka.Loggers.*` are used
to log messages to various destinations like the terminal or a file. This
can help to debug your application.

Each message has a source, a message type, and a severity:

| Source           | Type                 | Severity     |
|------------------|----------------------|--------------|
| Worker           | Error                | Error        |
| Game\_Loop       | Deprecated\_Behavior | Warning      |
| Resource\_Loader | Undefined\_Behavior  | Info         |
| OpenGL           | Portability          | Debug        |
| Window\_System   | Performance          |              |
| Shader\_Compiler | Other                |              |
| Third\_Party     | Marker               |              |
| Application      | Push\_Group          |              |
| Other            | Pop\_Group           |              |

## Loggers

A logger is an object that logs messages to some destination. For example,
the terminal or to a file. Loggers implement the synchronized interface
`Logger`, which allows messages to be logged from any task.

There are several implementations in the packages `:::ada Orka.Loggers.*`:

- **Terminal**. Prints messages to the terminal.

- **Location**. Logs messages to a file in a writable location.

The logger can be set by calling procedure `Set_Logger` in the package
`:::ada Orka.Logging`. The default logger is `:::ada Orka.Loggers.Terminal.Logger`.

### Terminal

The logger returned by the function `Logger` in the package
`:::ada Orka.Loggers.Terminal` logs messages to the standard error stream
of the terminal if the severity is `Error` or to the standard output of
the terminal for any lower severity.

The default logger returned by function `Logger` will log messages with
any severity. To only log messages with a certain minimum severity, create
a new logger:

```ada
Orka.Logging.Set_Logger (Orka.Loggers.Terminal.Create_Logger
  (Level => Orka.Loggers.Info));
```

### Location

The package `:::ada Orka.Loggers.Location` is a generic package that can be
used to create loggers that will write to a file in a writable location.
This package has two generic parameters:

- `Location`: of the type `Writable_Location_Ptr` (see [Locations][url-locations])

- `Capacity_Queue`: a `Positive` integer.

When the generic package is instantiated, a background task and a queue
bounded by `Capacity_Queue` will be created. The instantiated package
provides the procedure `Shutdown` to terminate this background task.
This procedure must be called at the end of your application. If the
task has been shutdown, or if the queue is full, new messages are not
dropped, but instead logged via `:::ada Orka.Loggers.Terminal.Logger`.

To create a logger that can log to a file in the writable location, execute
the function `Create_Logger` with the path to a file. An optional second
parameter can be given to specify the minimum severity of messages
(default is `Debug`).

!!! example
    To log to files in the directory `/var/log/orka`, the package can be
    instantiated at the library level:

    ```ada linenums="1"
    Location_Logs : constant Locations.Writable_Location_Ptr
      := Locations.Directories.Create_Location ("/var/log/orka");

    package Loggers_Location is new Orka.Loggers.Location
      (Location => Location_Logs, Capacity_Queue => 10);
    ```

    A logger that can log messages to a specific file in this location
    can then be created by calling `Create_Logger`:

    ```ada
    File_Logger : constant Orka.Loggers.Logger_Ptr
      := Loggers_Location.Create_Logger ("default.log");
    ```

    and then set as the default logger:

    ```ada
    Orka.Logging.Set_Logger (File_Logger);
    ```

## Logging

The package `:::ada Orka.Logging` provides subprograms to log messages.
These subprograms can be called from any task. To log a message, use the
procedure `Log`:

```ada
Orka.Logging.Log
  (From    => Application,
   Kind    => Other,
   Level   => Info,
   ID      => 0,
   Message => "FPS: " & FPS'Image);
```

The identifier should be unique per source and is an application-defined
number.

Alternatively, the generic package `Messages` can be instantiated:

```ada
use all type Orka.Logging.Source;
use all type Orka.Logging.Severity;
use Orka.Logging;

package Messages is new Orka.Logging.Messages (Game_Loop);
```

and then call `Log` of the instantiated package:

```ada
Messages.Log (Debug, "Simulation tick resolution: " & Trim (Image (Tick)));
```

This will log the following text:

`[00:00:13.370042 DEBUG] [GAME_LOOP:OTHER] 0: Simulation tick resolution: 0.001 us`

The generic package has an optional second generic parameter `ID`, which
is 0 by default.

### Formatting

To trim a string, use the function `Trim`.

To format a variable of type `:::ada Ada.Real_Time.Time_Span`, use the
function `Image`.

## OpenGL debugging

The graphics driver has the ability to notify the application of various
events that may occur. Each message has a source, message type, severity,
implementation-defined ID, and a human-readable description:

To make sure the driver generates messages, use the `Debug` flag when
initializing the windowing library:

```ada
Context : Orka.Contexts.Context'Class := Orka.Windows.GLFW.Create_Context
  (Version => (4, 3), Flags  => (Debug => True, others => False));
```

If `Debug` is `False` then the video driver may choose to not enable
full debug output.
To enable log messages from the video driver, call from the OpenGL task:

```ada
Orka.Debug.Set_Log_Messages (Enable => True);
```

This will flush all messages currently in the log and then
enables a callback which forwards all messages to the procedure `Log` in the
package `:::ada Orka.Logging`. This procedure commands the video driver
to generate messages with any severity, including `Debug`.

The callback may be called by the graphics driver from multiple tasks,
concurrently, and/or asynchronously after executing an OpenGL command
if `:::ada GL.Toggles.State (Debug_Output_Synchronous) = Disabled`.

If you would like to raise an exception if an OpenGL command fails, set
the parameter `Raise_API_Error` to `True`:

```ada
Orka.Debug.Set_Log_Messages (Enable => True, Raise_API_Error => True);
```

This will make debug output synchronously, so that it is only called
from the task holding the OpenGL context.

### Enabling or disabling specific messages

Messages of a specific source, type, level, or ID can be enabled or
disabled by calling one of the `:::ada GL.Debug.Set` procedures.

### Object labels

!!! note "TODO"

### Groups

!!! note "TODO"

  [url-locations]: /resources/locations
