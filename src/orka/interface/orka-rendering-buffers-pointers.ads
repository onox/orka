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

private package Orka.Rendering.Buffers.Pointers is
   pragma Preelaborate;

   package Half is new GL.Objects.Buffers.Buffer_Pointers
     (Half_Pointers);

   package Single is new GL.Objects.Buffers.Buffer_Pointers
     (Single_Pointers);

   package Double is new GL.Objects.Buffers.Buffer_Pointers
     (Double_Pointers);

   package Int is new GL.Objects.Buffers.Buffer_Pointers
     (Int_Pointers);

   package UInt is new GL.Objects.Buffers.Buffer_Pointers
     (UInt_Pointers);

   package Color is new GL.Objects.Buffers.Buffer_Pointers
     (Colors.Basic_Color_Pointers);

   package Single_Vector4 is new GL.Objects.Buffers.Buffer_Pointers
     (Orka.Types.Singles.Vector4_Pointers);

   package Double_Vector4 is new GL.Objects.Buffers.Buffer_Pointers
     (Orka.Types.Doubles.Vector4_Pointers);

   package Single_Matrix4 is new GL.Objects.Buffers.Buffer_Pointers
     (Orka.Types.Singles.Matrix4_Pointers);

   package Double_Matrix4 is new GL.Objects.Buffers.Buffer_Pointers
     (Orka.Types.Doubles.Matrix4_Pointers);

   package Arrays_Command is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Arrays_Indirect_Command_Pointers);

   package Elements_Command is new GL.Objects.Buffers.Buffer_Pointers
     (Indirect.Elements_Indirect_Command_Pointers);

end Orka.Rendering.Buffers.Pointers;
