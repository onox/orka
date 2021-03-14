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

with GL.Types;

with Orka.Rendering.Buffers;
with Orka.Types;

package Orka.Rendering.Drawing is
   pragma Preelaborate;

   procedure Draw
     (Mode          : GL.Types.Connection_Mode;
      Offset, Count : Natural;
      Instances     : Positive := 1);
   --  Draw primitives without any index buffer bound to the active
   --  vertex format

   procedure Draw_Indexed
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Offset, Count : Natural;
      Instances     : Positive := 1)
   with Pre => Index_Buffer.Kind in Types.Index_Type;
   --  Draw primitives using an index buffer bound to the active vertex
   --  format

   -----------------------------------------------------------------------------

   procedure Draw_Indirect
     (Mode       : GL.Types.Connection_Mode;
      Buffer     : Buffers.Buffer;
      Offset, Count : Natural);
   --  Draw multiple arrays commands at the given offset in the bound
   --  indirect buffer

   procedure Draw_Indirect
     (Mode   : GL.Types.Connection_Mode;
      Buffer : Buffers.Buffer);
   --  Draw all arrays commands in the bound indirect buffer

   procedure Draw_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Buffer, Count : Buffers.Buffer);
   --  Draw multiple arrays commands in the bound indirect buffer. The
   --  number of commands is determined by the value in the Count buffer

   -----------------------------------------------------------------------------

   procedure Draw_Indexed_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Buffer        : Buffers.Buffer;
      Offset, Count : Natural)
   with Pre => Index_Buffer.Kind in Types.Index_Type;
   --  Draw multiple elements commands at the given offset in the bound
   --  indirect buffer

   procedure Draw_Indexed_Indirect
     (Mode         : GL.Types.Connection_Mode;
      Index_Buffer : Buffers.Buffer;
      Buffer       : Buffers.Buffer)
   with Pre => Index_Buffer.Kind in Types.Index_Type;
   --  Draw all elements commands in the bound indirect buffer

   procedure Draw_Indexed_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Buffer, Count : Buffers.Buffer)
   with Pre => Index_Buffer.Kind in Types.Index_Type;
   --  Draw multiple elements commands in the bound indirect buffer. The
   --  number of commands is determined by the value in the Count buffer

end Orka.Rendering.Drawing;
