--------------------------------------------------------------------------------
-- Copyright (c) 2015 onox <denkpadje@gmail.com>
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

package GL.Transforms is
   pragma Preelaborate;

   use GL.Types;
   use Singles;

   procedure Translate (Matrix : in out Matrix4; Vector : Vector3);

   procedure Rotate_X (Matrix : in out Matrix4; Angle : Single);
   procedure Rotate_Y (Matrix : in out Matrix4; Angle : Single);
   procedure Rotate_Z (Matrix : in out Matrix4; Angle : Single);

   procedure Scale (Matrix : in out Matrix4; Vector : Vector3);

   function Perspective (FOV, Aspect, Z_Near, Z_Far : Single) return Matrix4;

end GL.Transforms;
