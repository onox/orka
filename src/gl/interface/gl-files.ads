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

with GL.Objects.Shaders;

-- This package is an addition to the original OpenGL API and simplifies
-- the handling of ressources located in files, like shader sources.
package GL.Files is
   pragma Preelaborate;

   procedure Load_Shader_Source_From_File (Object : Objects.Shaders.Shader;
                                           File_Name : String);

end GL.Files;
