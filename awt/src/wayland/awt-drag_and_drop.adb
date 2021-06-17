--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

with Ada.Streams;
with Ada.Unchecked_Conversion;

with Wayland.Enums.Client;
with Wayland.Protocols.Client;

with AWT.OS;
with AWT.Registry;

package body AWT.Drag_And_Drop is

   package WP renames Wayland.Protocols;
   package WE renames Wayland.Enums;

   Global : AWT.Registry.Compositor renames AWT.Registry.Global;

   task Drag_And_Drop is
      entry Receive (FD : Wayland.File_Descriptor; CB : not null Receive_Callback);
   end Drag_And_Drop;

   task body Drag_And_Drop is
      Process_FD    : Wayland.File_Descriptor;
      Process_CB    : Receive_Callback;
   begin
      loop
         select
            accept Receive (FD : Wayland.File_Descriptor; CB : not null Receive_Callback) do
               Process_FD := FD;
               Process_CB := CB;
            end Receive;

            declare
               File : AWT.OS.File (Process_FD);

               Received_Content : SU.Unbounded_String;
            begin
               loop
                  begin
                     declare
                        Result : constant Ada.Streams.Stream_Element_Array := File.Read;

                        subtype Byte_Array is Ada.Streams.Stream_Element_Array (Result'Range);

                        subtype Bytes_String is String (1 .. Result'Length);

                        function Convert is new Ada.Unchecked_Conversion
                          (Source => Byte_Array, Target => Bytes_String);
                     begin
                        exit when Result'Length = 0;

                        SU.Append (Received_Content, Convert (Result));
                     end;
                  exception
                     when Constraint_Error =>
                        File.Close;
                        raise;
                  end;
               end loop;

               File.Close;

               Process_CB (Received_Content);
            end;
         or
            terminate;
         end select;
      end loop;
   end Drag_And_Drop;

   function Supported_Actions return AWT.Inputs.Actions is
     (Global.Seat.Supported_Drag_Drop_Actions);

   function Valid_Action return AWT.Inputs.Action_Kind is
     (Global.Seat.Valid_Drag_Drop_Action);

   procedure Set_Action (Action : AWT.Inputs.Action_Kind) is
      Preferred_Action : WE.Client.Data_Device_Manager_Dnd_Action := (others => False);
   begin
      case Action is
         when Copy =>
            Preferred_Action.Copy := True;
         when Move =>
            Preferred_Action.Move := True;
         when Ask =>
            Preferred_Action.Ask := True;
         when None =>
            null;
      end case;

      Global.Seat.Data_Offer.Set_Actions (Preferred_Action, Preferred_Action);
   end Set_Action;

   procedure Finish (Action : AWT.Inputs.Action_Kind) is
   begin
      if Action /= None then
         if Valid_Action = Ask then
            Set_Action (Action);
         else
            case Action is
               when Copy =>
                  pragma Assert (Valid_Action = Copy);
               when Move =>
                  pragma Assert (Valid_Action = Move);
               when Ask | None =>
                  raise Program_Error;
            end case;
         end if;

         Global.Seat.Data_Offer.Finish;
      end if;

      Global.Seat.Data_Offer.Destroy;
   end Finish;

   procedure Get (Callback : not null Receive_Callback) is
      File_Descriptors : AWT.OS.Pipe;
   begin
      if not Global.Seat.Drag_Drop_Mime_Type_Valid or not Global.Seat.Data_Offer.Has_Proxy then
         raise Program_Error;
      end if;

      AWT.OS.Create_Pipe (File_Descriptors);

      Global.Seat.Data_Offer.Receive (AWT.Registry.URIs_Mime_Type, File_Descriptors.Write);
      declare
         File : AWT.OS.File (File_Descriptors.Write);
      begin
         File.Close;
      end;

      Global.Display.Roundtrip;

      Drag_And_Drop.Receive (File_Descriptors.Read, Callback);
   end Get;

   protected type Receive_Future is
      procedure Set (Value : SU.Unbounded_String);

      entry Get (Value : out SU.Unbounded_String);
   private
      Content : SU.Unbounded_String;
      Done    : Boolean := False;
   end Receive_Future;

   protected body Receive_Future is
      procedure Set (Value : SU.Unbounded_String) is
      begin
         Content := Value;
         Done    := True;
      end Set;

      entry Get (Value : out SU.Unbounded_String) when Done is
      begin
         Value := Content;
         Done  := False;

         Content := SU.Null_Unbounded_String;
      end Get;
   end Receive_Future;

   Future : Receive_Future;

   function Get return String is
   begin
      Get (Future.Set'Access);

      declare
         Value : SU.Unbounded_String;
      begin
         Future.Get (Value);

         return +Value;
      end;
   end Get;

end AWT.Drag_And_Drop;
