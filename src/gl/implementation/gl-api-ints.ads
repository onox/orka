--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

package GL.API.Ints is
   pragma Preelaborate;

   use GL.Types.Ints;

   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttribI1i", Attributes.Attribute, Int);

   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttribI2i", Attributes.Attribute, Int, Int);

   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI2iv", Attributes.Attribute, Vector2);

   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttribI3i", Attributes.Attribute, Int, Int, Int);

   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI3iv", Attributes.Attribute, Vector3);

   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttribI4i", Attributes.Attribute, Int, Int, Int, Int);

   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib4Iiv", Attributes.Attribute, Vector4);

end GL.API.Ints;
