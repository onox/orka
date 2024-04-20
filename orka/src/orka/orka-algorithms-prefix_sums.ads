--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Orka.Contexts;
with Orka.Rendering.Buffers;
with Orka.Resources.Locations;

private with Orka.Rendering.Shaders.Objects;
private with Orka.Types;

package Orka.Algorithms.Prefix_Sums is
   pragma Preelaborate;

   type Prefix_Sum (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited private;

   function Length (Object : Prefix_Sum) return Positive;

   procedure Compute_Prefix_Sum
     (Object : in out Prefix_Sum;
      Buffer : Rendering.Buffers.Buffer)
   with Pre => Buffer.Length = Object.Length;

   function Create_Prefix_Sum
     (Context  : aliased Orka.Contexts.Context'Class;
      Location : Resources.Locations.Location_Ptr;
      Length   : Positive) return Prefix_Sum
   with Pre => Length mod 4 = 0;

private

   type Prefix_Sum (Context : not null access constant Orka.Contexts.Context'Class) is tagged limited record
      Program_Prefix_Sum : Rendering.Shaders.Objects.Shader_Objects;
      Program_Add        : Rendering.Shaders.Objects.Shader_Objects;

      Length, Work_Groups, Sum_Work_Groups : Positive;
      Buffer_2, Buffer_3, Buffer_4         : Rendering.Buffers.Buffer (Types.UInt_Type);
   end record;

   function Length (Object : Prefix_Sum) return Positive is (Object.Length);

end Orka.Algorithms.Prefix_Sums;
