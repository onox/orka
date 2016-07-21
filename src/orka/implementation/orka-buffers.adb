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

package body Orka.Buffers is

   function Create_Buffer (Usage : GL.Objects.Buffers.Buffer_Usage) return Buffer is
   begin
      return Result : Buffer do
         Result.Usage := Usage;
         Result.Length := 0;
         Result.Buffer.Initialize_Id;
      end return;
   end Create_Buffer;

   function GL_Buffer (Object : Buffer) return GL.Objects.Buffers.Buffer
     is (Object.Buffer);

   function Length (Object : Buffer) return Natural
     is (Object.Length);

   procedure Set_Data (Object : in out Buffer; Data : Single_Array) is
      procedure Load is new GL.Objects.Buffers.Load_To_Buffer
        (GL.Types.Single_Pointers);
   begin
      Load (Object.Buffer, Data, Object.Usage);
      Object.Length := Data'Length;
   end Set_Data;

   procedure Set_Data (Object : in out Buffer; Data : UInt_Array) is
      procedure Load is new GL.Objects.Buffers.Load_To_Buffer
        (GL.Types.UInt_Pointers);
   begin
      Load (Object.Buffer, Data, Object.Usage);
      Object.Length := Data'Length;
   end Set_Data;

   procedure Set_Data (Object : in out Buffer; Data : Colors.Basic_Color_Array) is
      procedure Load is new GL.Objects.Buffers.Load_To_Buffer
        (GL.Types.Colors.Basic_Color_Pointers);
   begin
      Load (Object.Buffer, Data, Object.Usage);
      Object.Length := Data'Length;
   end Set_Data;

   procedure Set_Data (Object : in out Buffer; Data : Indirect.Arrays_Indirect_Command_Array) is
      procedure Load is new GL.Objects.Buffers.Load_To_Buffer
        (GL.Types.Indirect.Arrays_Indirect_Command_Pointers);
   begin
      Load (Object.Buffer, Data, Object.Usage);
      Object.Length := Data'Length;
   end Set_Data;

   procedure Set_Data (Object : in out Buffer; Data : Indirect.Elements_Indirect_Command_Array) is
      procedure Load is new GL.Objects.Buffers.Load_To_Buffer
        (GL.Types.Indirect.Elements_Indirect_Command_Pointers);
   begin
      Load (Object.Buffer, Data, Object.Usage);
      Object.Length := Data'Length;
   end Set_Data;

end Orka.Buffers;
