--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

generic
   type Element_Type is private;
   type Index_Type   is (<>);
package GL.Algebra is
   pragma Pure;

   -----------------------------------------------------------------------------
   --                              Vector types                               --
   -----------------------------------------------------------------------------

   type Vector2 is array (Index_2D) of aliased Element_Type;
   type Vector3 is array (Index_3D) of aliased Element_Type;
   type Vector4 is array (Index_Homogeneous) of aliased Element_Type;

   pragma Convention (C, Vector2);
   pragma Convention (C, Vector3);
   pragma Convention (C, Vector4);

   -----------------------------------------------------------------------------
   --                              Matrix types                               --
   -----------------------------------------------------------------------------

   type Matrix4 is array (Index_Homogeneous, Index_Homogeneous) of aliased Element_Type;

   pragma Convention (C, Matrix4);

   -----------------------------------------------------------------------------
   --                               Array types                               --
   -----------------------------------------------------------------------------

   type Vector2_Array is array (Index_Type range <>) of aliased Vector2;
   type Vector3_Array is array (Index_Type range <>) of aliased Vector3;
   type Vector4_Array is array (Index_Type range <>) of aliased Vector4;

   type Matrix4_Array is array (Index_Type range <>) of aliased Matrix4;

   pragma Convention (C, Vector2_Array);
   pragma Convention (C, Vector3_Array);
   pragma Convention (C, Vector4_Array);
   pragma Convention (C, Matrix4_Array);

end GL.Algebra;
