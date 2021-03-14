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

with GL.Types.Pointers;

with Orka.Types.Pointers;

private package Orka.Rendering.Buffers.Pointers is
   pragma Preelaborate;

   use GL.Types.Pointers;

   package Half is new GL.Objects.Buffers.Buffer_Pointers
     (Half_Pointers);

   package Single is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   package Double is new GL.Objects.Buffers.Buffer_Pointers
     (Double_Pointers);

   package Byte is new GL.Objects.Buffers.Buffer_Pointers
     (Byte_Pointers);

   package Short is new GL.Objects.Buffers.Buffer_Pointers
     (Short_Pointers);

   package Int is new GL.Objects.Buffers.Buffer_Pointers
     (Int_Pointers);

   package UByte is new GL.Objects.Buffers.Buffer_Pointers
     (UByte_Pointers);

   package UShort is new GL.Objects.Buffers.Buffer_Pointers
     (UShort_Pointers);

   package UInt is new GL.Objects.Buffers.Buffer_Pointers
     (UInt_Pointers);

   -----------------------------------------------------------------------------

   use Orka.Types.Pointers;

   package Single_Vector4 is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Vector4_Pointers);

   package Double_Vector4 is new GL.Objects.Buffers.Buffer_Pointers
     (Double_Vector4_Pointers);

   package Single_Matrix4 is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Matrix4_Pointers);

   package Double_Matrix4 is new GL.Objects.Buffers.Buffer_Pointers
     (Double_Matrix4_Pointers);

   -----------------------------------------------------------------------------

   package Arrays_Command is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Arrays_Indirect_Command_Pointers);

   package Elements_Command is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Elements_Indirect_Command_Pointers);

   package Dispatch_Command is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Dispatch_Indirect_Command_Pointers);

end Orka.Rendering.Buffers.Pointers;
