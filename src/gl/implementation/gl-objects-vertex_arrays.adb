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

with GL.API;
with GL.Low_Level;

package body GL.Objects.Vertex_Arrays is

   procedure Create (Object : in out Vertex_Array_Object) is
      New_Id : UInt := 0;
   begin
      API.Create_Vertex_Arrays.Ref (1, New_Id);
      Object.Reference.GL_Id := New_Id;

      API.Bind_Vertex_Array.Ref (Object.Reference.GL_Id);
   end Create;

   procedure Delete (Object : in out Vertex_Array_Object) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Vertex_Arrays.Ref (1, Arr);
      Object.Reference.GL_Id := 0;
   end Delete;

end GL.Objects.Vertex_Arrays;
