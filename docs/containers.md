# Containers

Orka provides a few containers that can be used as a simpler alternative
to the containers in the standard library of Ada.

## Bounded vectors

Type `Vector` in package `:::ada Orka.Containers.Bounded_Vectors` provides
a bounded vector backed by an array. The type provides constant and variable
indexing and can be iterated.

To create a vector containing elements of type `Element`, first instantiate
the generic package:

```ada
package Vectors is new Orka.Containers.Bounded_Vectors (Positive, Element);
```

### Creating a vector

After having instantiated the generic package, a vector can be created
with a specified maximum capacity:

```ada
Elements : Vectors.Vector (Capacity => Maximum_Elements);
```

To retrieve the current number of elements in the vector, use the
function `Length`. To know whether the vector is empty or at maximum
capacity, use the functions `Is_Empty` and `Is_Full`.

### Adding elements

Elements can only be appended to the end of the vector, they cannot be
inserted into arbitrary positions. An element can be appended to the
vectors as follows:

```ada
Elements.Append (New_Element);
```

You can also append a whole vector if the total length does not exceed
the capacity:

```ada
Elements.Append (Another_Elements_Vector);
```

### Removing elements

Only the last element can be removed from the vector:

```ada
declare
   Removed_Element : Element;
begin
   Elements.Remove_Last (Removed_Element);
end;
```

To remove all elements, call `Clear`:

```ada
Elements.Clear;
```

### Getting an element

To get the current value at a certain index, use Ada 2012's indexing
syntax:

```ada
Some_Element : constant Element := Elements (I);
```

or call function `Element`:

```ada
Some_Element : constant Element := Elements.Element (I);
```

If you need the whole vector as an array, use procedure `Query`:

```ada
declare
   procedure Use_Elements (Elements : Vectors.Element_Array) is
   begin
      --  Do something here with Elements
   end Use_Elements;
begin
   Elements.Query (Use_Elements'Access);
end;
```

Note that `Elements'Length` in the procedure is equal to the length of
the vector, not its capacity.

### Updating elements

To update an element at a certain index, use Ada 2012's variable indexing
syntax:

```ada
Elements (I) := Some_Element;
```

or call procedure `Update`:

```ada
declare
   procedure Update_Element (Element : in out Vector.Element_Type) is
   begin
      --  Write to Element here
   end Update_Element;
begin
   Elements.Update (I, Update_Element'Access);
end;
```

### Iterating over the elements

You can iterate over the elements in a vector by using Ada 2012's iterator
syntax:

```ada
for Element of Elements loop
   --  Do something here with Element
end loop;
```

Variable `Element` is not read-only and can be updated by assigning a
value to it.

## Ring buffers

Type `Buffer` in package `:::ada Orka.Containers.Ring_Buffers` provides
a ring buffer backed by an array. It does not support Ada 2012's indexing
and iterator syntax, and is supposed to be used to provide a FIFO queue
with a certain maximum capacity.

To create a ring buffer containing elements of type `Element`, first
instantiate the generic package:

```ada
package Buffers is new Orka.Containers.Ring_Buffers (Element);
```

### Creating a ring buffer

After having instantiated the generic package, a ring buffer can be created
with a specified maximum capacity:

```ada
Elements : Buffers.Buffer (Capacity => Maximum_Elements);
```

To retrieve the current number of elements in the buffer, use the
function `Length`. To know whether the buffer is empty or at maximum
capacity, use the functions `Is_Empty` and `Is_Full`.

### Appending an element

Elements can only be appended to the end of the buffer, they cannot be
inserted into arbitrary positions. An element can be appended to the
buffer as follows:

```ada
Elements.Append (New_Element);
```

### Removing the first element

To remove the first element, call function `Remove_First`:

```ada
Some_Element : constant Element := Elements.Remove_First;
```
