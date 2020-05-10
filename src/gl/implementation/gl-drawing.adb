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

with System;

with GL.API;
with GL.Low_Level;
with GL.Types.Indirect;

package body GL.Drawing is

   procedure Draw_Arrays
     (Mode          : Connection_Mode;
      Offset, Count : Size;
      Instances     : Size := 1;
      Base_Instance : Size := 0) is
   begin
      API.Draw_Arrays_Instanced_Base_Instance.Ref
        (Mode, Offset, Count, Instances, UInt (Base_Instance));
   end Draw_Arrays;

   procedure Draw_Multiple_Arrays_Indirect
     (Mode   : Connection_Mode;
      Count  : Size;
      Offset : Size := 0)
   is
      use GL.Types.Indirect;

      Offset_In_Bytes : constant Size
        := Offset * Arrays_Indirect_Command'Size / System.Storage_Unit;
   begin
      API.Multi_Draw_Arrays_Indirect.Ref (Mode, Offset_In_Bytes, Count, 0);
   end Draw_Multiple_Arrays_Indirect;

   procedure Draw_Multiple_Arrays_Indirect_Count
     (Mode      : Connection_Mode;
      Max_Count : Size;
      Offset, Count_Offset : Size := 0)
   is
      use GL.Types.Indirect;

      Offset_In_Bytes : constant Size
        := Offset * Arrays_Indirect_Command'Size / System.Storage_Unit;
      Count_Offset_In_Bytes : constant Size
        := Offset * Size'Size / System.Storage_Unit;
      pragma Assert (Count_Offset_In_Bytes mod 4 = 0);
   begin
      API.Multi_Draw_Arrays_Indirect_Count.Ref
        (Mode, Offset_In_Bytes, Low_Level.IntPtr (Count_Offset), Max_Count, 0);
   end Draw_Multiple_Arrays_Indirect_Count;

   procedure Draw_Elements
     (Mode          : Connection_Mode;
      Count         : Size;
      Index_Kind    : Index_Type;
      Index_Offset  : Natural;
      Instances     : Size := 1;
      Base_Instance : Size := 0;
      Base_Vertex   : Size := 0)
   is
      Element_Bytes : Natural;
   begin
      case Index_Kind is
         when UShort_Type => Element_Bytes := 2;
         when UInt_Type   => Element_Bytes := 4;
      end case;
      API.Draw_Elements_Instanced_Base_Vertex_Base_Instance.Ref
        (Mode, Count, Index_Kind, Low_Level.IntPtr (Element_Bytes * Index_Offset),
         Instances, Int (Base_Vertex), UInt (Base_Instance));
   end Draw_Elements;

   procedure Draw_Multiple_Elements_Indirect
     (Mode       : Connection_Mode;
      Index_Kind : Index_Type;
      Count      : Size;
      Offset     : Size := 0)
   is
      use GL.Types.Indirect;

      Offset_In_Bytes : constant Size
        := Offset * Elements_Indirect_Command'Size / System.Storage_Unit;
   begin
      API.Multi_Draw_Elements_Indirect.Ref
        (Mode, Index_Kind, Offset_In_Bytes, Count, 0);
   end Draw_Multiple_Elements_Indirect;

   procedure Draw_Multiple_Elements_Indirect_Count
     (Mode         : Connection_Mode;
      Index_Kind   : Index_Type;
      Max_Count    : Size;
      Offset, Count_Offset : Size := 0)
   is
      use GL.Types.Indirect;

      Offset_In_Bytes : constant Size
        := Offset * Elements_Indirect_Command'Size / System.Storage_Unit;
      Count_Offset_In_Bytes : constant Size
        := Offset * Size'Size / System.Storage_Unit;
      pragma Assert (Count_Offset_In_Bytes mod 4 = 0);
   begin
      API.Multi_Draw_Elements_Indirect_Count.Ref
        (Mode, Index_Kind, Offset_In_Bytes,
         Low_Level.IntPtr (Count_Offset_In_Bytes), Max_Count, 0);
   end Draw_Multiple_Elements_Indirect_Count;

end GL.Drawing;
