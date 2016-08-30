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

with GL.Attributes;

package GL.API.Shorts is
   pragma Preelaborate;
   
   use GL.Types.Shorts;
   
   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttrib1s", Attributes.Attribute, Short);
   
   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttrib2s", Attributes.Attribute, Short, Short);
   
   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib2sv", Attributes.Attribute, Vector2);
   
   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttrib3s", Attributes.Attribute, Short, Short, Short);
   
   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib3sv", Attributes.Attribute, Vector3);
   
   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttrib4s", Attributes.Attribute, Short, Short, Short, Short);
   
   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib4sv", Attributes.Attribute, Vector4);
   
end GL.API.Shorts;
