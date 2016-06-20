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

with GL.Types;

package GL.Drawing is
   pragma Preelaborate;

   use GL.Types;

   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count : Size);
   procedure Draw_Arrays (Mode : Connection_Mode; Offset, Count, Instances : Size);

   procedure Draw_Arrays_Base_Instance (Mode : Connection_Mode; Offset, Count : Size;
                                        Instances, Base_Instance : Size);

   procedure Draw_Multiple_Arrays (Mode : Connection_Mode; Offsets, Counts : Size_Array)
     with Pre => Offsets'Length = Counts'Length;

   procedure Draw_Elements (Mode : Connection_Mode; Count : Size;
                            Index_Type : Unsigned_Numeric_Type);
   procedure Draw_Elements (Mode : Connection_Mode; Count : Size;
                            Index_Type : Unsigned_Numeric_Type;
                            Instances  : Size);

   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Size;
                                        Index_Type : Unsigned_Numeric_Type;
                                        Vertex_Offset, Index_Offset : Int);
   procedure Draw_Elements_Base_Vertex (Mode : Connection_Mode; Count : Size;
                                        Index_Type : Unsigned_Numeric_Type;
                                        Instances  : Size;
                                        Vertex_Offset, Index_Offset : Int);

   procedure Draw_Elements_Base_Vertex_Base_Instance (Mode : Connection_Mode; Count : Size;
                                                      Index_Type : Unsigned_Numeric_Type;
                                                      Instances  : Size;
                                                      Vertex_Offset, Index_Offset : Int;
                                                      Base_Instance : Size);

   procedure Draw_Elements_Base_Instance (Mode : Connection_Mode; Count : Size;
                                          Index_Type : Unsigned_Numeric_Type;
                                          Instances  : Size;
                                          Index_Offset : Int;
                                          Base_Instance : Size);

   procedure Draw_Multiple_Elements (Mode : Connection_Mode;
                                     Index_Type : Unsigned_Numeric_Type;
                                     Counts, Index_Offsets : Size_Array)
     with Pre => Counts'Length = Index_Offsets'Length;

   procedure Draw_Multiple_Elements_Base_Vertex (Mode : Connection_Mode;
                                     Index_Type : Unsigned_Numeric_Type;
                                     Counts, Vertex_Offsets, Index_Offsets : Size_Array)
     with Pre => Counts'Length = Vertex_Offsets'Length and
                 Counts'Length = Index_Offsets'Length;

   procedure Draw_Multiple_Elements_Indirect (Mode : Connection_Mode;
                                              Index_Type : Unsigned_Numeric_Type;
                                              Count : Size);

end GL.Drawing;
