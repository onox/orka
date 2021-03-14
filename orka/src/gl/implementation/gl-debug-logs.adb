--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Deallocation;

with GL.API;
with GL.Debug_Types;
with GL.Enums.Getter;

package body GL.Debug.Logs is

   function Message_Log return Message_Array is
      use GL.Debug_Types;

      Length : Size := 0;

      Number_Messages : constant Size := Logged_Messages;
      Log_Length      : constant Size := Number_Messages * Max_Message_Length;

      Sources : Source_Array_Access   := new Source_Array (1 .. Number_Messages);
      Types   : Type_Array_Access     := new Type_Array (1 .. Number_Messages);
      Levels  : Severity_Array_Access := new Severity_Array (1 .. Number_Messages);

      IDs     : UInt_Array_Access := new UInt_Array (1 .. Number_Messages);
      Lengths : Size_Array_Access := new Size_Array (1 .. Number_Messages);

      Log : Debug_Types.String_Access := new String'(1 .. Natural (Log_Length) => ' ');

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => String, Name => Debug_Types.String_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Source_Array, Name => Source_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Type_Array, Name => Type_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Severity_Array, Name => Severity_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => UInt_Array, Name => UInt_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Size_Array, Name => Size_Array_Access);
   begin
      Length := Size (API.Get_Debug_Message_Log.Ref
        (UInt (Number_Messages), Log_Length,
         Sources, Types, IDs, Levels, Lengths, Log));
      pragma Assert (Length <= Number_Messages);

      declare
         Messages : Message_Array (1 .. Length);
         Offset : Natural := 1;
      begin
         for Index in 1 .. Length loop
            Messages (Index) :=
              (From    => Sources (Index),
               Kind    => Types (Index),
               Level   => Levels (Index),
               ID      => IDs (Index),
               Message => String_Holder.To_Holder
                 (Log (Offset .. Offset + Natural (Lengths (Index)) - 1)));
            Offset := Offset + Natural (Lengths (Index));
         end loop;

         Free (Sources);
         Free (Types);
         Free (Levels);
         Free (IDs);
         Free (Lengths);
         Free (Log);

         return Messages;
      end;
   end Message_Log;

   function Logged_Messages return Size is
      Result : Int := 0;
   begin
      API.Get_Integer.Ref (Enums.Getter.Debug_Logged_Messages, Result);
      return Result;
   end Logged_Messages;

end GL.Debug.Logs;
