--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Interfaces.C.Pointers;

package GL.Types.Indirect is
   pragma Preelaborate;

   type Single_Array_Access is access constant Single_Array;
   type UInt_Array_Access   is access constant UInt_Array;

   type Arrays_Indirect_Command is record
      Count, Instances, First_Vertex, Base_Instance : UInt;
   end record;

   type Elements_Indirect_Command is record
      Count, Instances, First_Index, Base_Vertex, Base_Instance : UInt;
   end record;

   type Arrays_Indirect_Command_Array is array (Size range <>)
     of aliased Arrays_Indirect_Command
     with Convention => C;

   type Elements_Indirect_Command_Array is array (Size range <>)
     of aliased Elements_Indirect_Command
     with Convention => C;

   package Arrays_Indirect_Command_Pointers is new Interfaces.C.Pointers
     (Size, Arrays_Indirect_Command,
      Arrays_Indirect_Command_Array, Arrays_Indirect_Command'(others => 0));

   package Elements_Indirect_Command_Pointers is new Interfaces.C.Pointers
     (Size, Elements_Indirect_Command,
      Elements_Indirect_Command_Array, Elements_Indirect_Command'(others => 0));

end GL.Types.Indirect;
