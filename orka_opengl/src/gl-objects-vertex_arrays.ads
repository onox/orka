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

package GL.Objects.Vertex_Arrays is
   pragma Preelaborate;

   type Vertex_Array_Object is new GL_Object with private;
   --  A single VAO is manually created and binded after the context
   --  is made current.

   procedure Create (Object : in out Vertex_Array_Object);
   procedure Delete (Object : in out Vertex_Array_Object);

   overriding
   procedure Initialize_Id (Object : in out Vertex_Array_Object) is null;
   --  Null because VAO is created manually

   overriding
   procedure Delete_Id (Object : in out Vertex_Array_Object) is null;
   --  Null because VAO is deleted manually

   overriding
   function Identifier (Object : Vertex_Array_Object) return Types.Debug.Identifier is
     (Types.Debug.Vertex_Array);

private

   type Vertex_Array_Object is new GL_Object with null record;

end GL.Objects.Vertex_Arrays;
