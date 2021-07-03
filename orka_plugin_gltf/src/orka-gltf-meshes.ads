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

with Orka.Containers.Bounded_Vectors;

package Orka.glTF.Meshes is
   pragma Preelaborate;

   subtype Primitive_Mode is GL.Types.Connection_Mode
     range GL.Types.Points .. GL.Types.Triangle_Strip;

   type Attribute_Kind is (Position, Normal, Texcoord_0);

   type Attribute_Array is array (Attribute_Kind) of Natural;

   type Primitive is record
      Attributes : Attribute_Array;
      Indices    : Natural_Optional;
      Material   : Natural_Optional;
      Mode       : Primitive_Mode;
   end record;

   type Mesh is record
      --  Orka.Resources.Models.glTF can handle only one primitive per mesh
      Primitives : Primitive;
      Name       : Name_Strings.Bounded_String;
   end record;

   package Mesh_Vectors is new Orka.Containers.Bounded_Vectors (Natural, Mesh);

   function Get_Meshes
     (Meshes : Types.JSON_Value) return Mesh_Vectors.Vector;

end Orka.glTF.Meshes;
