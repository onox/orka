# Windows and input devices

!!! note "TODO"

## Ada Window Toolkit

AWT is used to create windows that can display 3D graphics with
an OpenGL context and to manage input devices like the pointer, keyboard, and
gamepads. It has a similar purpose as GLFW and SDL.

Currently there is only a Wayland backend for Linux. Hopefully in the future a
backend for Windows will be added.

AWT also supports gamepads:

- **Mappings**. Use mappings from the [SDL gamecontroller database][url-sdl-gamecontroller-db].

- **Events**. Listen for (dis)connection events.

- **Force-feedback**. Play and cancel rumble and periodic force-feedback effects.

- **Motion sensor**. Get the linear acceleration and angular velocity using the motion
  sensor of a gamepad.

- **Battery**. Retrieve the capacity and charging state of the battery of a gamepad.

- **LED**. Get and set the color of the LED of a gamepad.

  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
