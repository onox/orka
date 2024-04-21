--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

private with Ada.Containers.Indefinite_Holders;

private with GL.Objects.Shaders;

package Orka.Rendering.Shaders.Modules is
   pragma Preelaborate;

   type Shader_Module is tagged private;

   type Shader_Module_Array is array (Positive range <>) of Shader_Module;

   function Kind (Object : Shader_Module) return Shader_Kind;

   function Create_Module
     (Kind : Shader_Kind;
      Path : String) return Shader_Module;

   function Create_Modules
     (Kind  : Shader_Kind;
      Paths : String_Array) return Shader_Module_Array;

   function Create_Module_From_Source
     (Kind : Shader_Kind;
      Text : String) return Shader_Module;

   -----------------------------------------------------------------------------
   --                                 Internal                                --
   -----------------------------------------------------------------------------

   procedure Attach_Shaders (Modules : Shader_Module_Array; Shader : Orka.Rendering.Shaders.Shader);

   procedure Detach_Shaders (Modules : Shader_Module_Array; Shader : Orka.Rendering.Shaders.Shader);

private

   use type GL.Objects.Shaders.Shader;

   package Shader_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.Shaders.Shader);

   type Shader_Module is tagged record
      Shader : Shader_Holders.Holder;
   end record;

end Orka.Rendering.Shaders.Modules;
