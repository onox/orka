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

with Wayland.Protocols.Client;

with AWT.OS;
with AWT.Registry;

package body AWT.Clipboard is

   package WP renames Wayland.Protocols;

   Global : AWT.Registry.Compositor renames AWT.Registry.Global;

   Content : SU.Unbounded_String;

   task Clipboard is
      entry Send (FD : Wayland.File_Descriptor; Value : SU.Unbounded_String);
      entry Receive (FD : Wayland.File_Descriptor; CB : not null Receive_Callback);
   end Clipboard;

   task body Clipboard is
      Process_FD    : Wayland.File_Descriptor;
      Process_Value : SU.Unbounded_String;
      Process_CB    : Receive_Callback;
   begin
      loop
         select
            accept Send (FD : Wayland.File_Descriptor; Value : SU.Unbounded_String) do
               Process_FD    := FD;
               Process_Value := Value;
            end Send;

            declare
               File : AWT.OS.File (Process_FD);
            begin
               File.Write (+Process_Value);
               File.Close;
            end;

            Process_Value := SU.Null_Unbounded_String;
         or
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
   end Clipboard;

   procedure Data_Source_Send
     (Data_Source : in out WP.Client.Data_Source'Class;
      Mime_Type   : String;
      FD          : Wayland.File_Descriptor) is
   begin
      if Mime_Type = AWT.Registry.UTF_8_Mime_Type then
         Clipboard.Send (FD, Content);
         --  Do not clear Content here so that we can send the content
         --  again if requested
      else
         declare
            File : AWT.OS.File (FD);
         begin
            File.Close;
         end;
      end if;
   end Data_Source_Send;

   procedure Data_Source_Cancelled
     (Data_Source : in out WP.Client.Data_Source'Class) is
   begin
      Data_Source.Destroy;
      Content := +"";
   end Data_Source_Cancelled;

   package Data_Source_Events is new WP.Client.Data_Source_Events
     (Send      => Data_Source_Send,
      Cancelled => Data_Source_Cancelled);

   procedure Set (Value : String) is
   begin
      if Global.Data_Source.Has_Proxy then
         Global.Data_Source.Destroy;
      end if;

      Global.Data_Device_Manager.Create_Data_Source (Global.Data_Source);

      if not Global.Data_Source.Has_Proxy then
         raise Internal_Error with "Wayland: Failed to get data source";
      end if;

      Content := +Value;

      Data_Source_Events.Subscribe (Global.Data_Source);

      Global.Data_Source.Offer (AWT.Registry.UTF_8_Mime_Type);

      Global.Seat.Data_Device.Set_Selection
        (Global.Data_Source, Global.Seat.Keyboard_Enter_Serial);
   end Set;

   procedure Get (Callback : not null Receive_Callback) is
      File_Descriptors : AWT.OS.Pipe;
   begin
      if not Global.Seat.Clipboard_Mime_Type_Valid or not Global.Seat.Data_Offer.Has_Proxy then
         Callback (+"");
         return;
      end if;

      AWT.OS.Create_Pipe (File_Descriptors);

      Global.Seat.Data_Offer.Receive (AWT.Registry.UTF_8_Mime_Type, File_Descriptors.Write);
      declare
         File : AWT.OS.File (File_Descriptors.Write);
      begin
         File.Close;
      end;

      Global.Display.Roundtrip;

      Clipboard.Receive (File_Descriptors.Read, Callback);
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

         --  The used Data_Offer will be destroyed when a new one comes
         --  in in AWT.Registry

         return +Value;
      end;
   end Get;

end AWT.Clipboard;
