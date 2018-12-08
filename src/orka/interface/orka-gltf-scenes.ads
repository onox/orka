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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Orka.Transforms.Singles.Matrices;

package Orka.glTF.Scenes is
   pragma Preelaborate;

   package Natural_Vectors is new Ada.Containers.Vectors (Natural, Natural);

   package Transforms renames Orka.Transforms.Singles.Matrices;

   type Transform_Kind is (Matrix, TRS);

   type Node (Transform : Transform_Kind) is record
      Name     : SU.Unbounded_String;
      Mesh     : Natural_Optional;
      Children : Natural_Vectors.Vector;
      case Transform is
         when Matrix =>
            Matrix : Transforms.Matrix4;
         when TRS =>
            Translation : Transforms.Vector4;
            Rotation    : Transforms.Vector4;
            Scale       : Transforms.Vector4;
      end case;
   end record;

   package Node_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Node);

   function Get_Nodes
     (Nodes : Types.JSON_Value) return Node_Vectors.Vector;

   type Scene is record
      Name  : SU.Unbounded_String;
      Nodes : Natural_Vectors.Vector;
   end record;

   package Scene_Vectors is new Ada.Containers.Vectors (Natural, Scene);

   function Get_Scenes
     (Scenes : Types.JSON_Value) return Scene_Vectors.Vector;

end Orka.glTF.Scenes;
