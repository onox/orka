--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with Orka.Rendering.Shaders.Modules;
with Orka.Types;

package Orka.Rendering.Shaders.Objects is
   pragma Preelaborate;

   package Optional_Shaders is new Orka.Types.Optionals (Shader);

   subtype Optional_Shader is Optional_Shaders.Optional;

   function From (Object : Shader) return Optional_Shader is (Is_Present => True, Value => Object);

   function Empty return Optional_Shader is (Is_Present => False);

   function Create_Shader
     (Kind : Shader_Kind;
      Path : String) return Optional_Shader
   is (From (Create_Shader (Kind, Path)));

   function Create_Shader_From_Source
     (Kind : Shader_Kind;
      Text : String) return Optional_Shader
   is (From (Create_Shader_From_Source (Kind, Text)));

   function Create_Shader_From_Files
     (Kind  : Shader_Kind;
      Paths : String_Array) return Optional_Shader
   is (if Paths'Length > 0 then From (Create_Shader (Kind, Paths)) else Empty);

   function Create_Shader (Modules : Orka.Rendering.Shaders.Modules.Shader_Module_Array) return Optional_Shader is
     (From (Create_Shader (Modules)));

   -----------------------------------------------------------------------------

   type Shader_Objects is array (Shader_Kind) of Optional_Shader
     with Dynamic_Predicate => (for all Kind in Shader_Objects'Range =>
                                  (if Shader_Objects (Kind).Is_Present then Shader_Objects (Kind).Value.Kind = Kind));

end Orka.Rendering.Shaders.Objects;
