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

with Orka.Resources.Locations;

package Orka.Rendering.Programs.Modules is
   pragma Preelaborate;

   type Module is tagged private;

   type Module_Array is array (Positive range <>) of aliased Module;

   function Create_Module_From_Sources (VS, TCS, TES, GS, FS, CS : String := "")
     return Module;
   --  Create a module containing shaders that have a non-empty source text

   function Create_Module
     (Location : Resources.Locations.Location_Ptr;
      VS, TCS, TES, GS, FS, CS : String := "") return Module;
   --  Create a module containing shaders that have a non-empty file path

   procedure Attach_Shaders (Modules : Module_Array; Program : in out Programs.Program);

   procedure Detach_Shaders (Modules : Module_Array; Program : Programs.Program);

   Shader_Compile_Error : exception;

   type Shader_Module is new Module with private;

   function Kind (Object : Shader_Module) return Shader_Kind;

   type Shader_Module_Array is array (Positive range <>) of aliased Shader_Module;

   function Create_Module
     (Location : Resources.Locations.Location_Ptr;
      Kind     : Shader_Kind;
      Path     : String) return Shader_Module;

   overriding
   function Create_Module_From_Sources (VS, TCS, TES, GS, FS, CS : String := "")
     return Shader_Module is (raise Program_Error);

   overriding
   function Create_Module
     (Location : Resources.Locations.Location_Ptr;
      VS, TCS, TES, GS, FS, CS : String := "") return Shader_Module is (raise Program_Error);

private

   use type GL.Objects.Shaders.Shader;

   package Shader_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => GL.Objects.Shaders.Shader);

   type Shader_Array is array (GL.Objects.Shaders.Shader_Type) of Shader_Holder.Holder;

   type Module is tagged record
      Shaders : Shader_Array;
   end record;

   type Shader_Module is new Module with record
      Kind : Shader_Kind;
   end record;

   function Kind (Object : Shader_Module) return Shader_Kind is (Object.Kind);

end Orka.Rendering.Programs.Modules;
