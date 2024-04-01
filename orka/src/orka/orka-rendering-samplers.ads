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

with GL.Objects.Samplers;
with GL.Types.Colors;

package Orka.Rendering.Samplers is
   pragma Preelaborate;

   use GL.Objects.Samplers;
   use GL.Types;
   use all type GL.Types.Compare_Function;
   use all type GL.Types.Colors.Border_Color;

   subtype Wrapping_Mode       is GL.Objects.Samplers.Wrapping_Mode;
   subtype Minifying_Function  is GL.Objects.Samplers.Minifying_Function;
   subtype Magnifying_Function is GL.Objects.Samplers.Magnifying_Function;

   type Wrapping_Mode_3D is array (Index_3D) of Wrapping_Mode;

   type Sampler_State is record
      Minifying_Filter   : Minifying_Function  := Nearest_Mipmap_Linear;
      Magnifying_Filter  : Magnifying_Function := Linear;

      Minimum_LoD        : Float_64 := -1000.0;
      Maximum_LoD        : Float_64 :=  1000.0;
      LoD_Bias           : Float_64 :=     0.0;
      Max_Anisotropy     : Float_64 :=     1.0;

      Wrapping : Wrapping_Mode_3D := [others => Repeat];

      Border_Color             : Colors.Border_Color := Transparent_Black;
      Compare_X_To_Texture     : Boolean             := False;
      Current_Compare_Function : Compare_Function    := LEqual;
   end record
     with Dynamic_Predicate => Sampler_State.Max_Anisotropy >= 1.0;

   type Sampler is tagged private;

   function Create_Sampler (State : Sampler_State) return Sampler;

   procedure Bind (Object : Sampler; Index : Natural);
   --  Bind the sampler to the binding point at the given index

private

   type Sampler is tagged record
      Sampler : GL.Objects.Samplers.Sampler;
   end record;

end Orka.Rendering.Samplers;
