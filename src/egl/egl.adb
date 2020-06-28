--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Ada.Strings.Fixed;

package body EGL is

   package SF renames Ada.Strings.Fixed;

   function Trim (Value : C.Strings.chars_ptr) return String is
     (SF.Trim (C.Strings.Value (Value), Ada.Strings.Right));

   function Extensions (Value : C.Strings.chars_ptr) return String_List is
      Extensions : constant String := Trim (Value);

      Index : Positive := Extensions'First;
   begin
      return Result : String_List (1 .. SF.Count (Extensions, " ") + 1) do
         for I in Result'First .. Result'Last - 1 loop
            declare
               Next_Index : constant Positive := SF.Index (Extensions, " ", Index + 1);
            begin
               Result (I) := SU.To_Unbounded_String (Extensions (Index .. Next_Index - 1));
               Index := Next_Index + 1;
            end;
         end loop;
         Result (Result'Last) := SU.To_Unbounded_String (Extensions (Index .. Extensions'Last));
      end return;
   end Extensions;

   function Has_Extension (Extensions : String_List; Name : String) return Boolean is
      use type SU.Unbounded_String;
   begin
      return (for some Extension of Extensions => Extension = Name);
   end Has_Extension;

   procedure Check_Extension (Extensions : String_List; Name : String) is
   begin
      if not Has_Extension (Extensions, Name) then
         raise Feature_Not_Supported with Name & " not supported";
      end if;
   end Check_Extension;

end EGL;
