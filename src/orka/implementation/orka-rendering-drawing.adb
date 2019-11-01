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

with GL.Drawing;
with GL.Objects.Buffers;

package body Orka.Rendering.Drawing is

   use GL.Types;

   procedure Draw (Mode : GL.Types.Connection_Mode; Offset, Count : Natural) is
   begin
      GL.Drawing.Draw_Arrays (Mode, Offset => Size (Offset), Count => Size (Count));
   end Draw;

   procedure Draw_Indexed
     (Mode          : GL.Types.Connection_Mode;
      Index_Kind    : Types.Index_Type;
      Offset, Count : Natural) is
   begin
      GL.Drawing.Draw_Elements
        (Mode,
         Count        => Size (Count),
         Index_Kind   => Orka.Types.Convert (Index_Kind),
         Index_Offset => Offset);
   end Draw_Indexed;

   procedure Draw_Indirect
     (Mode       : GL.Types.Connection_Mode;
      Index_Kind : Types.Index_Type;
      Buffer     : Buffers.Buffer;
      Offset, Count : Natural) is
   begin
      GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Buffer.GL_Buffer);
      GL.Drawing.Draw_Multiple_Elements_Indirect
        (Mode, Orka.Types.Convert (Index_Kind), Count => Size (Count), Offset => Size (Offset));
   end Draw_Indirect;

   procedure Draw_Indirect
     (Mode       : GL.Types.Connection_Mode;
      Index_Kind : Types.Index_Type;
      Buffer     : Buffers.Buffer) is
   begin
      Draw_Indirect (Mode, Index_Kind, Buffer, Offset => 0, Count => Buffer.Length);
   end Draw_Indirect;

   procedure Draw_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Index_Kind    : Types.Index_Type;
      Buffer, Count : Buffers.Buffer) is
   begin
      GL.Objects.Buffers.Draw_Indirect_Buffer.Bind (Buffer.GL_Buffer);
      GL.Objects.Buffers.Parameter_Buffer.Bind (Count.GL_Buffer);
      GL.Drawing.Draw_Multiple_Elements_Indirect_Count
        (Mode, Orka.Types.Convert (Index_Kind), GL.Types.Size (Buffer.Length));
   end Draw_Indirect;

end Orka.Rendering.Drawing;
