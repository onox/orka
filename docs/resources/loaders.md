# Loaders

While a `Location` is responsible for reading a resource from a location
and returning a pointer to a `Byte_Array`, a `Loader` is responsible for
transforming this `Byte_Array` to some meaningful object.
Package `:::ada Orka.Resources.Loaders` provides the limited interface `Loader`.

## Registering a loader

After a specific loader has been created, it can then be registered to load
files found in some location. For example, given the location `Location_Models`
which points to a location where models can be found, a glTF loader can be
added with:

```ada
Orka.Resources.Loader.Add_Location (Location_Models, Loader_glTF);
```

## Loading a resource

A resource can be loaded asynchronously using the function `Load` in the
package `:::ada Orka.Resources.Loader`:

```ada
Resource_Ref : Orka.Futures.Pointers.References
  := Orka.Resources.Loader.Load (Path => Model_Path).Get;
```

Function `Load` will return a smart pointer to an instance of the synchronized
interface `Future`. When the status of the future becomes `Done` the
resource has been loaded. The specific loader that loaded the resource
will hand over ownership of the resource to a manager so that it
can be retrieved.

## Managers

Package `:::ada Orka.Resources.Managers` provides the tagged type `Manager`
which can take ownership of resources. Call `Create_Manager` to create
an instance:

```ada
Manager : constant Managers.Manager_Ptr := Managers.Create_Manager;
```

The manager can be queried for the existence of a resource given its
path:

```ada
if Manager.Contains (Texture_Path) then
   --  Retrieve the texture here
end if;
```

To remove a resource, call `Remove_Resource`:

```ada
Manager.Remove_Resource (Texture_Path);
```

To retrieve a resource, the function `Resource` can be called which
returns a pointer to the limited interface `Resource`. This pointer
can be converted to a `Texture_Ptr` or a `Model_Ptr` for example:

```ada
Texture_1 : constant Textures.Texture_Ptr
  := Textures.Texture_Ptr (Manager.Resource (Texture_Path));
```

## Loaders

### glTF

Package `:::ada Orka.Resources.Models.glTF` provides a loader to load
[glTF][url-gltf] models. To create a loader you need a `Manager` to
take ownership of the loaded model:

```ada
Loader_glTF : constant Loaders.Loader_Ptr := Models.glTF.Create_Loader
  (Manager => Manager);
```

!!! note
    To use package `:::ada Orka.Resources.Models.glTF` add
    Alire crate orka\_plugins\_gltf to your list of dependencies:

    ```sh
    $ alr with orka_plugins_gltf
    ```

!!! example
    ```ada linenums="1"
    use Ada.Exceptions;
    use Ada.Real_Time;

    use Orka.Logging;
    use Orka.Resources;

    declare
       Model_Path   : constant String := "animals/orka.gltf";

       Resource_Ref : Orka.Futures.Pointers.References
         := Loader.Load (Path => Model_Path).Get;

       use type Orka.Futures.Status;

       Resource_Status : Orka.Futures.Status;
       Resource_Future : Orka.Futures.Future_Access := Resource_Ref.Value;
    begin
       select
          Resource_Future.Wait_Until_Done (Resource_Status);

          pragma Assert (Resource_Status = Orka.Futures.Done);
          pragma Assert (Manager.Contains (Model_Path));

          declare
             Model_1 : constant Models.Model_Ptr
               := Models.Model_Ptr (Manager.Resource (Model_Path));
          begin
             --  Do something with Model_1 here, like creating an instance
             --  and adding it to a scene
          end;
       or
          delay until Clock + Milliseconds (2_000);
          Orka.Logging.Insert_Message (Resource_Loader, Warning, 0,
            "Not completed loading: " & Resource_Ref.Current_Status'Image);
       end select;
    exception
       when Error : others =>
          Orka.Logging.Insert_Message (Resource_Loader, Error, 0,
            "Failed loading resource: " & Exception_Information (Error));
    end;
    ```

    If `Loader` cannot load the resource for any reason, then `Wait_Until_Done`
    will set `Resource_Status` to `Orka.Futures.Failed` and raise an exception.

!!! warning "Limitations of the glTF loader"
    The glTF loader is currently quite limited and supports only a subset
    of the specification:

    - Textures are not supported because glTF does not yet support KTX
      textures
    - Meshes must have exactly one primitive with three attributes
      (position, normal, and UV coordinates) and an index (which must be
      an unsigned integer)

    See #1 and #41.

### KTX

Package `:::ada Orka.Resources.Textures.KTX` provides a loader to load
[KTX][url-ktx] textures. To create a loader you need a `Manager` to take
ownership of the loaded texture:

```ada
Loader_KTX : constant Loaders.Loader_Ptr := Textures.KTX.Create_Loader
  (Manager => Manager);
```

  [url-gltf]: https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md
  [url-ktx]: https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
