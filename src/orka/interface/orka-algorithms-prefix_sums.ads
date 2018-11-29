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

with Orka.Rendering.Buffers;
with Orka.Resources.Locations;

private with Orka.Rendering.Programs;
private with Orka.Types;

package Orka.Algorithms.Prefix_Sums is
   pragma Preelaborate;

   type Factory is tagged private;

   function Create_Factory
     (Location : Resources.Locations.Location_Ptr) return Factory;

   -----------------------------------------------------------------------------

   type Prefix_Sum is tagged private;

   function Length (Object : Prefix_Sum) return Positive;

   procedure Compute_Prefix_Sum
     (Object : in out Prefix_Sum;
      Buffer : Rendering.Buffers.Buffer)
   with Pre => Buffer.Length = Object.Length;

   function Create_Prefix_Sum
     (Object : Factory;
      Length : Positive) return Prefix_Sum'Class
   with Pre => Length mod 4 = 0;

private

   type Factory is tagged record
      Program_Prefix_Sum : Rendering.Programs.Program;
      Program_Add        : Rendering.Programs.Program;
   end record;

   type Prefix_Sum is tagged record
      Programs : Factory;

      Length, Work_Groups, Sum_Work_Groups : Positive;
      Buffer_2, Buffer_3, Buffer_4         : Rendering.Buffers.Buffer (Types.UInt_Type);
   end record;

   function Length (Object : Prefix_Sum) return Positive is (Object.Length);

end Orka.Algorithms.Prefix_Sums;
