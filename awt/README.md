[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/awt.json)](https://alire.ada.dev/crates/awt.html)
[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)

# Ada Window Toolkit

Ada 2012 library for managing input devices and windows that can display 3D graphics.
It has a similar purpose as GLFW and SDL.

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

## Dependencies

On Linux you need Wayland 1.18 or higher.

## Using the library

On Linux you need to copy the data/99-leds.rules file to /etc/udev/rules.d/
in order to be able to set the LED color of a gamepad.

## License

AWT is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
