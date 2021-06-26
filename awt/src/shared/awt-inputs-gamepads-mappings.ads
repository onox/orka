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

private package AWT.Inputs.Gamepads.Mappings is
   pragma Preelaborate;

   type Platform_Kind is (Linux, Windows, Mac_OS_X, iOS, Android);

   procedure Set_Mappings (Platform : Platform_Kind; Text : String);

   function Contains (GUID : GUID_String) return Boolean;

   function Get (GUID : GUID_String) return String;

   function Name_To_Output (Name : String) return Output_Mapping;

end AWT.Inputs.Gamepads.Mappings;
