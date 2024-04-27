# Locations

Resources may be loaded from arbitrary locations such as folders,
archives, removable media, or remote locations accessible over a
network. To provide access to these locations without coupling the
application to specific locations, the package `:::ada Orka.Resources.Locations`
provides the limited interfaces `Location` and `Writable_Location`.
Instances of these interfaces can be injected as dependencies into
subsystems that need to load resources.

An instance of a `Location` provides read-only access to files, while a
`Writable_Location` provides read and write access. An instance of a
`Location` is accessed via a `Location_Ptr` and an instance of a
`Writable_Location` is accessed via a `Writable_Location_Ptr`.

## Checking whether a file exists

To check if a file exists in a certain location, call the function `Exists`:

```ada
if Location_Models.Exists (Path => "animals/orka.gltf") then
   --  The model can be loaded
end if;
```

## Reading a file

To read a file identified by a path and get a pointer to a `Byte_Array`,
call the function `Read_Data`:

```ada
declare
   Data : constant Orka.Resources.Byte_Array_Pointers.Pointer
     := Location_Resources.Read_Data (Path => "shaders/model.frag");
begin
   Shader.Set_Source (Orka.Resources.Convert (Data.Get));
end;
```

Package `:::ada Orka.Resources` provides the utility function `Convert`
to do an unchecked conversion from `Byte_Array` to `String`.

## Writing to a file

Writing data to a file in a location requires the location instance
to implement the `Writable_Location` interface. Given an instance of
a writable location, data can be written to a file identified by a path
using the `Write_Data` procedure:

```ada
declare
   Bytes : constant Orka.Resources.Byte_Array_Pointers.Pointer := ...
begin
   Location_Screenshots.Write_Data (Path => "screenshot.ktx", Data => Bytes.Get);
end;
```

The file will be created if necessary.

!!! tip
    To save a `Texture` object to a file, use the procedure `Write_Texture`
    in the package `:::ada Orka.Resources.Textures.KTX` to write the texture
    to a .ktx file in a writable location. This procedure will internally
    call `Write_Data` to save the KTX data to the specified file.

## Locations

### Directories

Package `:::ada Orka.Resources.Locations.Directories` provides a location
object that provides read and/or write access to files in a folder on a
file system.

The function `Create_Location` can either return a `Location_Ptr` for
read-only access or a `Writable_Location_Ptr` for read and write access.
The function must be given the path to a folder:

```ada
use Orka.Resources;

Location_Resources : constant Locations.Location_Ptr
  := Locations.Directories.Create_Location ("path/to/resources");

Location_Screenshots : constant Locations.Writable_Location_Ptr
  := Locations.Directories.Create_Location ("path/to/screenshots");
```

### Archives

Package `:::ada Orka.Resources.Locations.Archives` provides a location
object that provides read-only access to files in a document container
file, a Zip-based archive format.

To create a location backed by a Zip file, execute the function
`Create_Location` giving it a path to a Zip file:

```ada
use Orka.Resources;

Location_Resources : constant Locations.Location_Ptr
  := Locations.Archives.Create_Location ("path/to/resources.zip");
```

!!! note
    To use package `:::ada Orka.Resources.Locations.Archives` add
    Alire crate orka\_plugins\_archives to your list of dependencies:

    ```sh
    $ alr with orka_plugins_archives
    ```

## Registering a location

Once a location object has been created, it should be registered so that
various objects can find their needed shaders and/or other files without
having to pass around the location object all the time.

There are a few namespace which are used by various crates of Orka and must be registered
if these crates are used in your application:

1. `orka`. Should point to the shaders of the crate `orka`.

2. `orka-atmosphere`. Should point to the shaders of the crate `orka_plugin_atmosphere`.

3. `orka-terrain`. Should point to the shaders of the crate `orka_plugin_terrain`.

4. `orka-tensors-gpu`. Should point to the shaders of the crate `orka_tensors_gpu`.

To register a namespace, invoke the procedure `Register` in the package `:::ada Orka.Resources.Locations`:

```ada
declare
   use Orka.Resources.Locations;
begin
   Register ("orka", Directories.Create_Location ("path/to/orka/data/shaders"));
   Register ("orka-atmosphere", Directories.Create_Location ("path/to/orka_plugin_atmosphere/data/shaders/atmosphere"));
   Register ("orka-terrain", Directories.Create_Location ("path/to/orka_plugin_terrain/data/shaders/terrain"));
   Register ("orka-tensors-gpu", Directories.Create_Location ("path/to/orka_tensors_gpu/data/shaders"));
end;
```
