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

package GL.API.Doubles is
   pragma Preelaborate;

   use GL.Types.Doubles;

   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttribL1d", Attributes.Attribute, Double);

   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttribL2d", Attributes.Attribute, Double, Double);

   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL2dv", Attributes.Attribute, Vector2);

   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttribL3d", Attributes.Attribute, Double, Double, Double);

   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL3dv", Attributes.Attribute, Vector3);

   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttribL4d", Attributes.Attribute, Double, Double, Double, Double);

   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL4dv", Attributes.Attribute, Vector4);

end GL.API.Doubles;
