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

with Orka.Types;

package Orka.Rendering.Programs.Shaders is
   pragma Preelaborate;

   package Optional_Shader_Programs is new Orka.Types.Optionals (Shader_Program);

   subtype Optional_Shader_Program is Optional_Shader_Programs.Optional;

   type Shader_Programs is array (Shader_Kind) of Optional_Shader_Program
     with Dynamic_Predicate => (for all Kind in Shader_Programs'Range =>
                                  (if Shader_Programs (Kind).Is_Present then Shader_Programs (Kind).Value.Kind = Kind));

   function From (Object : Shader_Program) return Optional_Shader_Program is (Is_Present => True, Value => Object);

   function Empty return Optional_Shader_Program is (Is_Present => False);

   function Create_Program
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Path     : String) return Optional_Shader_Program
   is (From (Create_Program (Location, Kind, Path)));

   function Create_Program_From_Source
     (Kind : Shader_Kind;
      Text : String) return Optional_Shader_Program
   is (From (Create_Program_From_Source (Kind, Text)));

   function Create_Program_From_Shaders
     (Location : Orka.Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Paths    : String_Array) return Optional_Shader_Program
   is (if Paths'Length > 0 then From (Create_Program (Location, Kind, Paths)) else Empty);

end Orka.Rendering.Programs.Shaders;
