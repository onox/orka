# Gamepads

Besides the pointer and the keyboard, AWT also supports external devices
like gamepads, including the following features:

- **Mappings**. Use mappings from the [SDL gamecontroller database][url-sdl-gamecontroller-db].

- **Events**. Listen for (dis)connection events.

- **Force-feedback**. Play and cancel rumble and periodic force-feedback effects.

- **Motion sensor**. Get the estimated orientation or measured linear acceleration
  and angular velocity using the motion sensor of a gamepad.

- **Battery**. Retrieve the capacity and charging state of the battery of a gamepad.

- **LED**. Get and set the color of the LED of a gamepad.

After the package `:::ada AWT.Inputs.Gamepads` has been initialized, an array
of `Gamepad` objects can be obtained with the function `Gamepads`. Some basic
information about a gamepad can be printed to the default logger with the
procedure `Log_Information`:

```ada
for Gamepad of AWT.Inputs.Gamepads.Gamepads loop
   Gamepad.Log_Information;
end loop;
```

The functions `Name`, `Serial_Number`, and `GUID`, return the name, serial number
and the 32 character GUID of the gamepad. Function `Connection` returns the
values `Disconnected`, `Wired`, or `Wireless`.

## Initialization

To initialize the currently connected gamepads and to make sure that gamepads
that are connected by the user later on get initialized as well, call
procedure `Initialize`:

```ada
AWT.Inputs.Gamepads.Initialize;
```

Before calling this procedure, however, load button and axis mappings first.

### Mappings

To properly initialize gamepads, button and axis mappings are needed from
the [SDL gamecontroller database][url-sdl-gamecontroller-db]. These mappings
should be loaded before the gamepads are initialized by the procedure `Initialize`.

Download the *gamecontrollerdb.txt* file and store it in some folder, for example,
in *data/*. Then create a `Location` object for this folder:

```ada
Location_Data : constant Orka.Resources.Locations.Location_Ptr :=
  (Orka.Resources.Locations.Directories.Create_Location ("data/"));
```

See [Locations](/resources/locations/) for more information about loading
resources from a location and a list of implementations that implement
the `Location` interface.

Next, read the file and call `Set_Mapping`, giving it the content of the read file:

```ada
if Location_Data.Exists ("gamecontrollerdb.txt") then
   AWT.Inputs.Gamepads.Set_Mappings
     (Orka.Resources.Convert (Location_Data.Read_Data ("gamecontrollerdb.txt").Get));
end if;
```

### Initialize gamepads

After having loaded the mappings, procedure `Initialize` must be called to
make sure that current and future connected gamepads get initialized.

## Updating state

To update the state of the gamepad, the hardware must be polled regularly
at a fixed interval.
This interval may be as small as something like a few milliseconds, especially
if a gamepad has a motion sensor and state estimation of the pose should be done.
To poll the hardware of the connected gamepads, execute procedure `Poll`:

```ada
AWT.Inputs.Gamepads.Poll (DT => 0.004);
```

Parameter `DT` may be 0.0 if parts of the state that depend on knowing how often
the procedure is called, such as pose estimation, are not needed.

Because the hardware is polled at a different frequency than the main event
loop for window, pointer, and keyboard events, the polling should happen in a
separate task:

```ada
task body Gamepads_Poller is
   Interval : constant Time_Span := Milliseconds (4);
   DT       : constant Duration  := To_Duration (Interval);

   Next_Time : Time := Clock + Interval;
begin
   loop
      exit when Window.Should_Close;

      AWT.Inputs.Gamepads.Poll (DT);

      Process_State_Of (AWT.Inputs.Gamepads.Gamepads);

      delay until Next_Time;
      Next_Time := Next_Time + Interval;
   end loop;
end Gamepads_Poller;
```

where `Process_State_Of` is a user-defined procedure that does something with
an array of `Gamepad` objects.

## State

The state of the buttons, axes, and triggers can be retrieved using the overloaded
function `State`:

```ada
State : constant AWT.Inputs.Gamepads.Gamepad_State := Gamepad.State;
```

A `Gamepad_State` object has a few components related to the analog sticks
and triggers:

- `Axes`. An array of `Axis_Position`, a fixed-point value in [-1.0, 1.0).

- `Triggers`. An array of `Trigger_Position`, a fixed-point value in [0.0, 1.0).

And a few related to the digital buttons:

- `Buttons`. An array of `Button_State` (value is `Released` or `Pressed`), where
  each state represents the current state of a particular button.

- `Pressed`. An array of `Boolean`, where each `True` indicates that a button's
  state has transitioned from `Released` to `Pressed`.

- `Released`. An array of `Boolean`, where each `True` indicates that a button's
  state has transitioned from `Pressed` to `Released`.

The arrays `Pressed` and `Released` are particularly useful to perform actions
that must happen just once. Polling the hardware will reset the values in these
two arrays; the next value of a button whose current value is `True` in the array
`Pressed`, will be `False` again.

For example, a rumble effect can be uploaded and played when the user presses
the right shoulder button, and canceled when the user releases the button:

```ada
if State.Pressed (Shoulder_Right) then
   Gamepad.Play_Effect (Effect_Fire_Weapon);
elsif State.Released (Shoulder_Right) then
   Gamepad.Cancel_Effect (Effect_Fire_Weapon);
end if;
```

While the array `Buttons` is useful for detecting if a button is currently held
down in a pressed state:

```ada
Options_Visible : constant Boolean := State.Buttons (Center_Right) = Pressed;
```

!!! tip
    The values of the enum types `Gamepad_Button`, `Gamepad_Axis`, and
    `Gamepad_Trigger` can be made visible with:

    ```ada
    use all type AWT.Inputs.Gamepads.Gamepad_Button;
    use all type AWT.Inputs.Gamepads.Gamepad_Axis;
    use all type AWT.Inputs.Gamepads.Gamepad_Trigger;
    ```

## Battery

The state of a battery can be retrieved with the overloaded function `State`:

```ada
Battery : constant AWT.Inputs.Gamepads.Battery_State := Gamepad.State;
```

The discriminant `Is_Present` is `True` if a battery is present, and `False` otherwise.
If present, the `Capacity` gives a whole number between 0 and 100.
The component `Status` has the value `Discharging`, `Charging`, or `Not_Charging`.

## LED

The state of a LED can be retrieved with the overloaded function `State`:

```ada
LED : constant AWT.Inputs.Gamepads.LED_State := Gamepad.State;
```

If a LED is present, then `Is_Present` will have the value `True`, and `False` if
the gamepad has no LED.
The component `Brightness` gives a normalized value between 0.0 and 1.0.
The `Color` is an array of normalized values for `Red`, `Green`, and `Blue`.

### Changing the color

The color and brightness can be set with the procedure `Set_LED`:

```ada
Gamepad.Set_LED
  (Brightness => 0.9,
   Color      => (Red => 0.8, Green => 0.1, Blue => 0.0));
```

## Motion sensor

The state of the motion sensor can be retrieved with the overloaded function `State`:

```ada
Motion : constant AWT.Inputs.Gamepads.Motion_State := Gamepad.State;
```

If a gamepad has a motion sensor, then `Is_Present` will return `True`,
otherwise it will return `False`.
If a motion sensor is present, then the component `Axes` will contain the raw values.
`X`, `Y`, and `Z` are the measured acceleration values in *g*'s,
where *g* is the gravitational acceleration constant (1 g = 9.81 m/s^2^).
`Rx`, `Ry`, and `Rz` are the measured angular velocity in degrees per second.

If the hardware has been polled with a `DT` greater than 0.0, then `Has_Pose` will
be `True` as well, otherwise it will be `False`.
The estimated true angular velocity (pitch up, yaw left, roll left) in
radians per second is then stored in `Angular_Velocity`,
while the estimated orientation is stored as a quaternion in the component `Orientation`.

## Force-feedback

Some gamepads support force-feedback. Usually only a limited number of effects
can be stored on the device. The function `Effects` returns a `Natural` number
that tells how many effects can be stored.
If the application tries to play too many effects at once, the oldest one will
be removed to make space on the device.

First, create an `Effect` object and store it somewhere at the library level.
An effect can then be played with the procedure `Play_Effect` and canceled
with the procedure `Cancel_Effect`:

```ada
if State.Pressed (Shoulder_Right) then
   Gamepad.Play_Effect (Effect_Fire_Weapon);
elsif State.Released (Shoulder_Right) then
   Gamepad.Cancel_Effect (Effect_Fire_Weapon);
end if;
```

It is not needed to continuously call `Play_Effect` to play an effect
for its whole duration.
It is sufficient and required to call the procedure just once when
it should start playing after being triggered by some activation condition.
The effect will be automatically uploaded to the gamepad if necessary.

### Effects

The following effects are supported:

- Rumble

- Periodic

!!! warning "Do not unnecessarily recreate `Effect` objects"
    The `Effect` object returned by one of the functions mentioned below
    is partially stored in some hidden data structure. Therefore, do not
    unnecessarily recreate these objects.
    Instead, create the effects once and store them somewhere at the library level.

#### Rumble

The function `Rumble_Effect` returns an effect that has a `Strong` and
`Weak` magnitude, both normalized values between 0.0 and 1.0.
The first two parameters are the `Length` and `Offset` of the effect and
specify the `Duration` of the effect and the delay before the effect starts.

For example, a rumble effect of 0.15 seconds that starts immediately and
has a strong magnitude of 0.7 and weak magnitude of 1.0, is created with:

```ada
Effect_Rumble : constant AWT.Inputs.Gamepads.Effect :=
  AWT.Inputs.Gamepads.Rumble_Effect (0.15, 0.0, 0.7, 1.0);
```

#### Periodic

A periodic effect can be created with the function `Periodic_Effect`. Just like
function `Rumble_Effect`, the first two parameters are the `Length` and `Offset`.
The remaining three parameters specify the `Magnitude`, `Attack`, and `Fade`.
`Magnitude` is a normalized value, and `Attack` and `Fade` are both of the type
`Duration` and specify how long it will take to transition to the full magnitude
and back to zero magnitude.

For example, to create an effect that takes 3 seconds to go to a magnitude of 0.8,
stay at that level for another 3 seconds, and then fade to zero in 2 seconds, use
the following code:

```ada
Effect_Periodic : constant AWT.Inputs.Gamepads.Effect :=
  AWT.Inputs.Gamepads.Periodic_Effect (8.0, 0.0, 0.8, 3.0, 2.0);
```

## Connection events

Just like monitors, gamepads may be connected or disconnected from the system
at arbitrary times. A listener object can be used to listen for these events.
Extend the type `Gamepad_Event_Listener` and override the
procedures `On_Connect` and `On_Disconnect`:

```ada
type Event_Listener is new AWT.Inputs.Gamepads.Gamepad_Event_Listener with null record;

overriding
procedure On_Connect
  (Object  : Event_Listener;
   Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr);

overriding
procedure On_Disconnect
  (Object  : Event_Listener;
   Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr);
```

and implement the overridden procedures:

```ada
overriding
procedure On_Connect
  (Object  : Event_Listener;
   Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr) is
begin
   Gamepad.Log_Information;
end On_Connect;

overriding
procedure On_Disconnect
  (Object  : Event_Listener;
   Gamepad : AWT.Inputs.Gamepads.Gamepad_Ptr) is
begin
   Gamepad.Log_Information;
end On_Disconnect;
```

and then create an object of the type to start listening for events:

```ada
Gamepad_Listener : Event_Listener;
```

As long as the object exists, the overridden procedures will be called when
a gamepad is connected or disconnected.

  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
