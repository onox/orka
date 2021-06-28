[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/awt.json)](https://alire.ada.dev/crates/awt.html)
[![License](https://img.shields.io/github/license/onox/awt.svg?color=blue)](https://github.com/onox/awt/blob/master/LICENSE)
[![GitHub release](https://img.shields.io/github/release/onox/awt.svg)](https://github.com/onox/awt/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# Ada Window Toolkit

Ada 2012 library for managing input devices and windows that can display 3D graphics.
It has a similar purpose as GLFW and SDL. To display graphics with OpenGL you need
to use [Orka][url-orka].

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

In order to build awt, you need to have:

 * An Ada 2012 compiler

 * [Alire][url-alire]

On Linux:

 * Wayland 1.16 or higher

## Using the library

Clone this repository, [orka][url-orka], and [wayland-ada][url-wayland-ada],
and make sure they are all in the same folder. Then add AWT to your application:

```sh
$ alr with awt --use=path/to/awt
```

On Linux you need to copy the data/99-leds.rules file to /etc/udev/rules.d/
in order to be able to set the LED color of a gamepad.

## Contributing

Please read the [contributing guidelines][url-contributing] before opening
issues or pull requests.

## License

AWT is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
  [url-orka]: https://github.com/onox/orka
  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
  [url-wayland-ada]: https://github.com/onox/wayland-ada
