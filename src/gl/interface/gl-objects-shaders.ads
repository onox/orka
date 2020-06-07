--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

private with GL.Low_Level;

package GL.Objects.Shaders is
   pragma Preelaborate;
   
   type Shader_Type is (Fragment_Shader, Vertex_Shader, Geometry_Shader,
                        Tess_Evaluation_Shader, Tess_Control_Shader,
                        Compute_Shader);
   
   type Shader (Kind : Shader_Type) is new GL_Object with private;

   procedure Set_Source (Subject : Shader; Source : String);
   function Source (Subject : Shader) return String;

   procedure Compile (Subject : Shader);

   function Compile_Status (Subject : Shader) return Boolean;

   function Info_Log (Subject : Shader) return String;

   overriding
   procedure Initialize_Id (Object : in out Shader);

   overriding
   procedure Delete_Id (Object : in out Shader);

   overriding
   function Identifier (Object : Shader) return Types.Debug.Identifier is
     (Types.Debug.Shader);

private

   type Shader (Kind : Shader_Type) is new GL_Object with null record;

   for Shader_Type use (Fragment_Shader        => 16#8B30#,
                        Vertex_Shader          => 16#8B31#,
                        Geometry_Shader        => 16#8DD9#,
                        Tess_Evaluation_Shader => 16#8E87#,
                        Tess_Control_Shader    => 16#8E88#,
                        Compute_Shader         => 16#91B9#);
   for Shader_Type'Size use Low_Level.Enum'Size;

end GL.Objects.Shaders;
