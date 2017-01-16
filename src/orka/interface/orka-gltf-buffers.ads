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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with GL.Low_Level.Enums;

with Orka.Resources;

package Orka.glTF.Buffers is
   pragma Preelaborate;

   use Orka.Resources;

   type Buffer_Kind is new GL.Low_Level.Enums.Buffer_Kind
     range GL.Low_Level.Enums.Array_Buffer .. GL.Low_Level.Enums.Element_Array_Buffer;

   Target_Kinds : constant array (Integer range <>) of Buffer_Kind :=
     (34962 => Buffer_Kind (GL.Low_Level.Enums.Array_Buffer),
      34963 => Buffer_Kind (GL.Low_Level.Enums.Element_Array_Buffer));

   type Buffer is record
      Data   : Byte_Array_Access;
      Kind   : Buffer_Kind;   --  TODO Do we really need Kind?
   end record;

   package Buffer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Buffer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Buffer_View is record
      Buffer : Byte_Array_Access;
      Offset : Natural;  --  Offset in bytes
      Length : Natural;  --  Length in bytes
      Target : Buffer_Kind;
   end record;

   package Buffer_View_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Buffer_View,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   function Create_Buffer (URI : String; Length : Natural) return Buffer
     with Post => Create_Buffer'Result.Data'Length = Length;

   function Create_Buffer_View
     (Buffer         : not null Byte_Array_Access;
      Offset, Length : Natural;
      Kind           : Buffer_Kind) return Buffer_View
     with Pre => Offset + Length <= Buffer.all'Length;

   function Elements (View : Buffer_View) return Byte_Array_Access;

end Orka.glTF.Buffers;
