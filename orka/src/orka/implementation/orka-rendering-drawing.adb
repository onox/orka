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

package body Orka.Rendering.Drawing is

   use GL.Types;

   procedure Draw
     (Mode          : GL.Types.Connection_Mode;
      Offset, Count : Natural;
      Instances     : Positive := 1) is
   begin
      GL.Drawing.Draw_Arrays
        (Mode,
         Offset    => Size (Offset),
         Count     => Size (Count),
         Instances => Size (Instances));
   end Draw;

   procedure Draw_Indexed
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Offset, Count : Natural;
      Instances     : Positive := 1)
   is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      Index_Buffer.Bind (Index);
      GL.Drawing.Draw_Elements
        (Mode,
         Count        => Size (Count),
         Index_Kind   => Orka.Types.Convert (Index_Buffer.Kind),
         Index_Offset => Offset,
         Instances    => Size (Instances));
   end Draw_Indexed;

   -----------------------------------------------------------------------------

   procedure Draw_Indirect
     (Mode       : GL.Types.Connection_Mode;
      Buffer     : Buffers.Buffer;
      Offset, Count : Natural)
   is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      Buffer.Bind (Draw_Indirect);
      GL.Drawing.Draw_Multiple_Arrays_Indirect
        (Mode, Count => Size (Count), Offset => Size (Offset));
   end Draw_Indirect;

   procedure Draw_Indirect
     (Mode   : GL.Types.Connection_Mode;
      Buffer : Buffers.Buffer) is
   begin
      Draw_Indirect (Mode, Buffer, Offset => 0, Count => Buffer.Length);
   end Draw_Indirect;

   procedure Draw_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Buffer, Count : Buffers.Buffer)
   is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      Buffer.Bind (Draw_Indirect);
      Count.Bind (Parameter);
      GL.Drawing.Draw_Multiple_Arrays_Indirect_Count
        (Mode, GL.Types.Size (Buffer.Length));
   end Draw_Indirect;

   -----------------------------------------------------------------------------

   procedure Draw_Indexed_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Buffer        : Buffers.Buffer;
      Offset, Count : Natural)
   is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      Index_Buffer.Bind (Index);
      Buffer.Bind (Draw_Indirect);
      GL.Drawing.Draw_Multiple_Elements_Indirect
        (Mode, Orka.Types.Convert (Index_Buffer.Kind),
         Count => Size (Count), Offset => Size (Offset));
   end Draw_Indexed_Indirect;

   procedure Draw_Indexed_Indirect
     (Mode         : GL.Types.Connection_Mode;
      Index_Buffer : Buffers.Buffer;
      Buffer       : Buffers.Buffer) is
   begin
      Draw_Indexed_Indirect (Mode, Index_Buffer, Buffer, Offset => 0, Count => Buffer.Length);
   end Draw_Indexed_Indirect;

   procedure Draw_Indexed_Indirect
     (Mode          : GL.Types.Connection_Mode;
      Index_Buffer  : Buffers.Buffer;
      Buffer, Count : Buffers.Buffer)
   is
      use all type Rendering.Buffers.Buffer_Target;
   begin
      Index_Buffer.Bind (Index);
      Buffer.Bind (Draw_Indirect);
      Count.Bind (Parameter);
      GL.Drawing.Draw_Multiple_Elements_Indirect_Count
        (Mode, Orka.Types.Convert (Index_Buffer.Kind), GL.Types.Size (Buffer.Length));
   end Draw_Indexed_Indirect;

end Orka.Rendering.Drawing;
