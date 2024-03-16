--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with GL.Low_Level;
with GL.Rasterization;
with GL.Types.Colors;

package GL.Buffers is
   pragma Preelaborate;

   use GL.Types;

   type Buffer_Bits is record
      Depth   : Boolean := False;
      Stencil : Boolean := False;
      Color   : Boolean := False;
   end record;

   subtype Depth is Single range 0.0 .. 1.0;

   subtype Stencil_Index is Int;

   type Stencil_Action is (Zero, Invert, Keep, Replace,
                           Increment, Decrement,
                           Increment_Wrap, Decrement_Wrap);

   type Color_Buffer_Selector is (None, Front_Left,
                                  Front_Right, Back_Left, Back_Right,
                                  Front, Back, Left, Right, Front_And_Back,
                                  Color_Attachment0,
                                  Color_Attachment1,
                                  Color_Attachment2,
                                  Color_Attachment3,
                                  Color_Attachment4,
                                  Color_Attachment5,
                                  Color_Attachment6,
                                  Color_Attachment7,
                                  Color_Attachment8,
                                  Color_Attachment9,
                                  Color_Attachment10,
                                  Color_Attachment11,
                                  Color_Attachment12,
                                  Color_Attachment13,
                                  Color_Attachment14,
                                  Color_Attachment15);

   --  Defined here because of following subtype declaration
   for Color_Buffer_Selector use (None           => 0,
                                  Front_Left     => 16#0400#,
                                  Front_Right    => 16#0401#,
                                  Back_Left      => 16#0402#,
                                  Back_Right     => 16#0403#,
                                  Front          => 16#0404#,
                                  Back           => 16#0405#,
                                  Left           => 16#0406#,
                                  Right          => 16#0407#,
                                  Front_And_Back => 16#0408#,
                                  Color_Attachment0  => 16#8CE0#,
                                  Color_Attachment1  => 16#8CE1#,
                                  Color_Attachment2  => 16#8CE2#,
                                  Color_Attachment3  => 16#8CE3#,
                                  Color_Attachment4  => 16#8CE4#,
                                  Color_Attachment5  => 16#8CE5#,
                                  Color_Attachment6  => 16#8CE6#,
                                  Color_Attachment7  => 16#8CE7#,
                                  Color_Attachment8  => 16#8CE8#,
                                  Color_Attachment9  => 16#8CE9#,
                                  Color_Attachment10 => 16#8CEA#,
                                  Color_Attachment11 => 16#8CEB#,
                                  Color_Attachment12 => 16#8CEC#,
                                  Color_Attachment13 => 16#8CED#,
                                  Color_Attachment14 => 16#8CEE#,
                                  Color_Attachment15 => 16#8CEF#);
   for Color_Buffer_Selector'Size use Low_Level.Enum'Size;

   subtype Draw_Buffer_Index is UInt range 0 .. 7;
   --  OpenGL specification defines 16, but no GPU supports more than 8

   type Color_Buffer_List is array (Draw_Buffer_Index range <>)
     of Color_Buffer_Selector;

   subtype Default_Color_Buffer_Selector is Color_Buffer_Selector
     range Front_Left .. Back_Right;
   --  Table 17.4 of the OpenGL specification

   subtype Explicit_Color_Buffer_Selector is Color_Buffer_Selector
     range Color_Attachment0 .. Color_Attachment15;
   --  Table 17.5 of the OpenGL specification

   subtype Single_Face_Selector is Rasterization.Face_Selector
     range Rasterization.Front .. Rasterization.Back;

   -----------------------------------------------------------------------------
   --                                  Depth                                  --
   -----------------------------------------------------------------------------

   procedure Set_Depth_Function (Func : Compare_Function);

   procedure Set_Depth_Mask (Enabled : Boolean);

   -----------------------------------------------------------------------------
   --                                  Color                                  --
   -----------------------------------------------------------------------------

   procedure Set_Color_Mask (Value : Colors.Enabled_Color);
   --  Set the color mask for all draw buffers

   procedure Set_Color_Mask
     (Index : Draw_Buffer_Index;
      Value : Colors.Enabled_Color);
   --  Set the color mask for a particular draw buffer

   -----------------------------------------------------------------------------
   --                                 Stencil                                 --
   -----------------------------------------------------------------------------

   procedure Set_Stencil_Function
     (Face : Rasterization.Face_Selector;
      Func : Compare_Function;
      Ref  : Int;
      Mask : UInt);

   procedure Set_Stencil_Operation
     (Face         : Rasterization.Face_Selector;
      Stencil_Fail : Buffers.Stencil_Action;
      Depth_Fail   : Buffers.Stencil_Action;
      Depth_Pass   : Buffers.Stencil_Action);

   procedure Set_Stencil_Mask
     (Value : UInt;
      Face  : Rasterization.Face_Selector := Rasterization.Front_And_Back);

private

   for Buffer_Bits use record
      Depth   at 0 range 8 .. 8;
      Stencil at 0 range 10 .. 10;
      Color   at 0 range 14 .. 14;
   end record;
   for Buffer_Bits'Size use Low_Level.Bitfield'Size;

   for Stencil_Action use (Zero           => 0,
                           Invert         => 16#150A#,
                           Keep           => 16#1E00#,
                           Replace        => 16#1E01#,
                           Increment      => 16#1E02#,
                           Decrement      => 16#1E03#,
                           Increment_Wrap => 16#8507#,
                           Decrement_Wrap => 16#8508#);
   for Stencil_Action'Size use Low_Level.Enum'Size;

   pragma Convention (C, Color_Buffer_List);

end GL.Buffers;
