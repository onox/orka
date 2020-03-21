--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with GL.Types;

with Orka.Transforms.SIMD_Matrices;

generic
   with package Transforms is new Orka.Transforms.SIMD_Matrices (<>);
package Orka.Algebra is
   pragma Preelaborate;

   subtype Matrix4 is Transforms.Matrix4;
   subtype Vector4 is Transforms.Vector4;

   function Identity4 return Matrix4 renames Transforms.Identity_Value;

   function Zero4 return Vector4 renames Transforms.Zero_Point;

   type Vector4_Array is array (GL.Types.Size range <>) of aliased Vector4
     with Convention => C;

   type Matrix4_Array is array (GL.Types.Size range <>) of aliased Matrix4
     with Convention => C;

end Orka.Algebra;
