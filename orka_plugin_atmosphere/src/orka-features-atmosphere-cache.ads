--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2023 onox <denkpadje@gmail.com>
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

with Orka.Behaviors;
with Orka.Cameras;
with Orka.Rendering.Programs.Modules;
with Orka.Resources.Locations;

with Orka.Features.Atmosphere.Rendering;

package Orka.Features.Atmosphere.Cache is

   type Cached_Atmosphere is tagged limited private;

   function Create_Atmosphere
     (Data             : aliased Model_Data;
      Location_Shaders : Resources.Locations.Location_Ptr;
      Location_Cache   : Resources.Locations.Writable_Location_Ptr;
      Parameters       : Rendering.Model_Parameters := (others => <>)) return Cached_Atmosphere;

   function Shader_Module (Object : Cached_Atmosphere)
     return Orka.Rendering.Programs.Modules.Module;
   --  Return the shader module for use by other features like terrain
   --  rendering

   procedure Render
     (Object : in out Cached_Atmosphere;
      Camera : Cameras.Camera_Ptr;
      Planet : Behaviors.Behavior_Ptr;
      Star   : Behaviors.Behavior_Ptr);
   --  Render the atmosphere on the far plane

private

   type Cached_Atmosphere is tagged limited record
      Atmosphere : Rendering.Atmosphere;
      Textures   : Precomputed_Textures;
   end record;

end Orka.Features.Atmosphere.Cache;
