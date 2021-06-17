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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Ada.Characters.Latin_1;

with Orka.Strings;
with Orka.Logging;

package body AWT.Inputs.Gamepads.Mappings is

   use all type Orka.Logging.Source;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   package Messages is new Orka.Logging.Messages (Window_System);

   package L1 renames Ada.Characters.Latin_1;

   function GUID_Hash (Key : GUID_String) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (String (Key)));

   package Mapping_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => GUID_String,
      Element_Type    => String,
      Hash            => GUID_Hash,
      Equivalent_Keys => "=");

   Mappings : Mapping_Maps.Map;

   procedure Set_Mappings (Platform : Platform_Kind; Text : String) is
      Lines : constant Orka.Strings.String_List := Orka.Strings.Split (Text, "" & L1.LF);

      Platform_Value : constant String :=
        (case Platform is
           when Linux    => "Linux",
           when Windows  => "Windows",
           when Mac_OS_X => "Mac OS X",
           when iOS      => "iOS",
           when Android  => "Android");
      Platform_String : constant String := ",platform:" & Platform_Value & ",";

      Count : Natural := 0;

      use type SU.Unbounded_String;
   begin
      for Line of Lines loop
         declare
            Mapping : constant Orka.Strings.String_List := Orka.Strings.Split (+Line, ",", 2);
         begin
            if Mapping'Length = 2 and SU.Index (Line, "#") /= 1 then
               declare
                  GUID  : constant String := (+Mapping (1));
                  Value : constant String := (+Mapping (2));

                  Value_Without_Platform : String renames
                    Value (Value'First .. Value'Last - Platform_String'Length);
                  Platform : String renames
                    Value (Value'Last - Platform_String'Length + 1 .. Value'Last);

                  pragma Assert (GUID = "xinput" or GUID'Length = GUID_String'Length);
               begin
                  if Platform = Platform_String and GUID /= "xinput" then
                     Mappings.Insert (GUID_String (GUID), Value_Without_Platform);
                     Count := Count + 1;
                  end if;
               end;
            end if;
         end;
      end loop;

      Messages.Log (Debug,
        "Added " & Trim (Count'Image) & " gamepad mappings for " & Platform_Value & " platform");
   end Set_Mappings;

   function Contains (GUID : GUID_String) return Boolean is
     (Mappings.Contains (GUID));

   function Get (GUID : GUID_String) return String is (Mappings (GUID));

end AWT.Inputs.Gamepads.Mappings;
