--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Orka.Transforms.Singles.Vectors;
with Orka.Types;

package Orka.Features.Terrain.Spheres is
   pragma Preelaborate;

   use all type GL.Types.Single;

   function Plane_To_Sphere
     (Vertex     : Orka.Transforms.Singles.Vectors.Vector4;
      Parameters : Orka.Features.Terrain.Spheroid_Parameters)
   return Orka.Transforms.Singles.Vectors.Vector4;

   function Get_Sphere_Visibilities
     (Parameters : Spheroid_Parameters;
      Front, Back, World, View : Orka.Types.Singles.Matrix4)
   return GL.Types.Single_Array
     with Post => Get_Sphere_Visibilities'Result'Length = 8;

   function Get_Visible_Tiles
     (Visibilities : GL.Types.Single_Array) return Visible_Tile_Array
   with Pre  => Visibilities'Length = 8,
        Post => Get_Visible_Tiles'Result'Length = 6;

end Orka.Features.Terrain.Spheres;
