# Atmosphere

The Alire crate orka\_plugins\_atmosphere provides several Ada packages
which can be used to render the atmosphere of the Earth.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Features.Atmosphere` and its child packages.

Import the following packages:

```ada
with Orka.Features.Atmosphere.Earth;
with Orka.Features.Atmosphere.Rendering;
with Orka.Features.Atmosphere.Cache;
```

## Creating an atmosphere

To render the atmosphere of the Earth, first create a location object
pointing to the shaders of the crate:

```ada
Location_Atmosphere : constant Locations.Location_Ptr :=
  Locations.Directories.Create_Location ("path/to/orka_plugin_atmosphere/data/shaders");
```

A second location, one which is writable is needed to cache the precomputed
textures:

```ada
Location_Cache : constant Locations.Writable_Location_Ptr :=
  Locations.Directories.Create_Location ("cache/atmosphere");
```

To create an atmosphere, some data about the atmosphere is needed.
Call the function `Data` in the package `:::ada Earth` to create
an object containing data about the model of the atmosphere:

```ada
Earth_Data : aliased constant Model_Data :=
  Earth.Data (Luminance => Approximate);
```

The parameter `Luminance` can contain the values `None`, `Approximate`, or `Precomputed`.
The value `Precomputed` causes 5 times more wavelengths to be precomputed than
the other two. It gives the highest accuracy of the luminance, but takes much
more time to compute than `Approximate`.

After the location objects have been created, create the `Cached_Atmosphere`
object by calling the function `Create_Atmosphere`:

```ada
Atmosphere : Cache.Cached_Atmosphere :=
  Cache.Create_Atmosphere
    (Earth_Data, Location_Atmosphere, Location_Cache);
```

A fourth parameter, `Parameters`, is optional and only needed if the planet is
slightly flattened and the `Flattening` component of the parameters is greater than zero.
The precomputed atmosphere assumes the planet has no flattening (for space/time
complexity reasons), but in reality the semi-minor axis of the Earth (center to
northpole) is 0.3 % of the semi-major axis (center to equator).
This gives a difference of about 20 km. The tessellated terrain can handle flattening,
so a hack is applied when the camera is near the surface to partially fix the
atmosphere near the horizon.

The parameters can be specified as following:

```ada
--  Based on WGS 84. See https://en.wikipedia.org/wiki/World_Geodetic_System#WGS84
Earth_Parameters : constant Rendering.Model_Parameters :=
  (Semi_Major_Axis => 6_378_137.0,
   Flattening      => 1.0 / 298.257_223_563,
   Axial_Tilt      => Orka.Transforms.Doubles.Vectors.To_Radians (23.439_2811),
   Star_Radius     => <>);
```

## Rendering

To render the atmosphere, call the procedure `Render`:

```ada
Atmosphere.Render (Camera, Planet, Sun);
```

The `Camera` needs to be a `Camera_Ptr` (defined in package `:::ada Orka.Cameras`)
and `Planet` and `Sun` need to be pointers to two objects implementing
the interface `Behavior` (defined in package `:::ada Orka.Behaviors`).

### Screenshots

Outside the atmosphere:

![Atmosphere dark](../images/atmosphere-dark.png)

Inside the atmosphere:

![Atmosphere light low](../images/atmosphere-light-low.png)
