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

with GL.Objects.Buffers;
with GL.Types.Indirect;
with GL.Types.Colors;

package Orka.Buffers is
   pragma Preelaborate;

   use GL.Types;

   type Buffer is tagged private;

   function Create_Buffer (Usage : GL.Objects.Buffers.Buffer_Usage) return Buffer;

   function GL_Buffer (Object : Buffer) return GL.Objects.Buffers.Buffer
     with Inline;

   function Length (Object : Buffer) return Natural
     with Inline;

   procedure Set_Data (Object : in out Buffer; Data : Single_Array);
   procedure Set_Data (Object : in out Buffer; Data : UInt_Array);

   procedure Set_Data (Object : in out Buffer; Data : Colors.Basic_Color_Array);

   procedure Set_Data (Object : in out Buffer; Data : Indirect.Arrays_Indirect_Command_Array);
   procedure Set_Data (Object : in out Buffer; Data : Indirect.Elements_Indirect_Command_Array);

private

   type Buffer is tagged record
      Buffer : GL.Objects.Buffers.Buffer;
      Usage  : GL.Objects.Buffers.Buffer_Usage;
      Length : Natural;
   end record;

end Orka.Buffers;
