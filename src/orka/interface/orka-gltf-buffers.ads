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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

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
      Length : Positive;  --  Length in bytes
   end record;

   package Buffer_Vectors is new Ada.Containers.Vectors (Natural, Buffer);

   function Get_Buffers
     (Buffers : Types.JSON_Array_Value) return Buffer_Vectors.Vector;

   subtype Stride_Natural is Natural range 4 .. 252;

   type Buffer_View (Packed : Boolean) is record
      Buffer : Byte_Array_Access;
      Offset : Natural;   --  Offset in bytes
      Length : Positive;  --  Length in bytes
      Target : Buffer_Kind;
      case Packed is
         when False =>
            Stride : Stride_Natural;
         when others =>
            null;
      end case;
   end record;

   package Buffer_View_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, Buffer_View);

   generic
      type Element_Type is private;
      type Element_Array is array (GL.Types.Size range <>) of aliased Element_Type;
   procedure Extract_From_Buffer
     (View  : Buffer_View;
      Data  : out Element_Array);

   function Get_Buffer_Views
     (Buffers : Buffer_Vectors.Vector;
      Views   : Types.JSON_Array_Value) return Buffer_View_Vectors.Vector;

end Orka.glTF.Buffers;
