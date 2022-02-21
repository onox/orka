# Pointer and keyboard

When querying the pointer and keyboard state, it is useful to make the
`Button_State` visible:

```ada
use all type AWT.Inputs.Button_State;
```

## Pointer

The state of the pointer can be retrieved with the overloaded function `State`:

```ada
Pointer : constant AWT.Inputs.Pointer_State := Window.State;

use all type AWT.Inputs.Pointer_Button;
use all type AWT.Inputs.Pointer_Mode;
use all type AWT.Inputs.Dimension;
```

A `Pointer_State` contains quite a few components:

- `Focused`. A `Boolean` which is `True` if the pointer has entered the
  window from which the pointer state was retrieved.

- `Scrolling`. A `Boolean` which is `True` if the user is scrolling in the window.

- `Mode`. An enum with the values `Visible`, `Hidden`, or `Locked`. The pointer
  can be locked to keep it from leaving the window. The mode can be changed with
  the procedure `Window.Set_Pointer_Mode`.

- `Cursor`. Contains the current cursor of the pointer. The cursor can be changed
  with the procedure `Window.Set_Pointer_Cursor`.

!!! tip
    If `Focused` is `True` and `Scrolling` has transitioned from `True` to `False`, then,
    and only then, the application may activate kinetic scrolling.

The following components are related to the pointer buttons `Left`, `Right`, and `Middle`:

- `Buttons`. An array of `Button_State` (value is `Released` or `Pressed`), where
  each state represents the current state of a particular button.

- `Pressed`. An array of `Boolean`, where each `True` indicates that a button's
  state has transitioned from `Released` to `Pressed`.

- `Released`. An array of `Boolean`, where each `True` indicates that a button's
  state has transitioned from `Pressed` to `Released`.

The following components are related to the position of the pointer and the scroll wheel:

- `Position`. The absolute position of the pointer w.r.t. the upper-left corner of the window.
  If the `Mode` is `Locked` then the `Position` does not change.

- `Relative`. The change in position relative to the previous state. This component
  changes only when `Mode` is `Locked`, otherwise it's value is 0.0 for both `X` and `Y`.
  If raw pointer motion has been enabled for a window with procedure `Set_Raw_Pointer_Motion`,
  then the unaccelerated delta's will be stored in the component.

- `Scroll`. The change in the position of the scroll wheel.

### Locking the position

The pointer can be hidden and locked to the current position in a window
by calling the procedure `Set_Pointer_Mode`.
This can be used, for example, to rotate the camera around an object
while holding down the right pointer button:

```ada
Rotate_Camera : constant Boolean :=
  Pointer.Focused and Pointer.Buttons (Right) = Pressed;

Mode : constant AWT.Inputs.Pointer_Mode :=
  (if Rotate_Camera then Locked else Visible);
```

The pointer mode can then be switched between `Visible` and `Locked`:

```ada
if Mode /= Pointer.Mode then
   Window.Set_Pointer_Mode (Mode);
end if;
```

### Changing the cursor

The current cursor of the pointer while it resides in some window can be
changed with the procedure `Set_Pointer_Cursor`:

```ada
Cursor : AWT.Inputs.Cursors.Pointer_Cursor :=
  (if Loading_Data then Wait else Default);
```

The cursor of the pointer can then be changed:

```ada
if Cursor /= Pointer.Cursor then
   Window.Set_Pointer_Cursor (Cursor);
end if;
```

!!! note
    Avoid repeatedly setting the same cursor over and over again.

#### Cursors

Besides the `Default` cursor, the following additional cursors can be set:

- Links and status: `Context_Menu`, `Help`, `Pointer`, `Progress`, `Wait`.

- Text selection: `Cell`, `Crosshair`, `Text`, `Vertical_Text`.

- Drag and drop: `Alias`, `Copy`, `Move`, `No_Drop`, `Not_Allowed`, `Grab`, `Grabbing`.

- Resizing and scrolling:
  `All_Scroll`, `Row_Resize`, `Col_Resize`,
  `N_Resize`, `E_Resize`, `S_Resize`, `W_Resize`,
  `NE_Resize`, `NW_Resize`, `SE_Resize`, `SW_Resize`, `EW_Resize`, `NS_Resize`,
  `NESW_Resize`, `NWSE_Resize`.

- Zooming: `Zoom_In`, `Zoom_Out`.

## Keyboard

The state of the keyboard can be retrieved with the overloaded function `State`:

```ada
Keyboard : constant AWT.Inputs.Keyboard_State := Window.State;

use all type AWT.Inputs.Keyboard_Button;
```

The `Keyboard_State` contains the following components:

- `Modifiers`. The modifier keys like ++shift++, ++ctrl++, etc. that are currently pressed.

- `Focused`. A `Boolean` that is `True` if the window has keyboard focus, and
  `False` otherwise.

The following components are related to the keys:

- `Buttons`. An array of `Button_State` (value is `Released` or `Pressed`), where
  each state represents the current state of a particular key.

- `Pressed`. An array of `Boolean`, where each `True` indicates that a key's
  state has transitioned from `Released` to `Pressed`.

- `Released`. An array of `Boolean`, where each `True` indicates that a key's
  state has transitioned from `Pressed` to `Released`.

Several components are to be used when implementing repeating keys:

- `Last_Pressed`. The last pressed key on the keyboard.

- `Repeat_Rate`. The number of characters per second when repeating a key.

- `Repeat_Delay`. The `Duration` after holding the last pressed key when
  repeating should start.

!!! bug "Limitations of keyboard support"
    Some functionality related to keyboards has not been implemented yet:

    - UTF-8 text input.

    - Retrieving the name key based on the `Keyboard_Button` and current keyboard layout.

!!! info
    For a full list of the keyboard keys, see type `Keyboard_Button` in
    package `:::ada AWT.Inputs`.

#### Modifiers

The following modifier keys exist in the `Modifiers` component of `Keyboard_State`:

- `Shift`
- `Caps_Lock`
- `Ctrl`
- `Alt`
- `Num_Lock`
- `Logo`
