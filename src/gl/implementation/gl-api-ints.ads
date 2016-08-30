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
