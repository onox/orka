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

with Ada.Unchecked_Deallocation;

package body EGL.Objects is

   overriding procedure Initialize (Object : in out EGL_Object) is
   begin
      Object.Reference := new EGL_Object_Reference'
        (ID     => ID_Type (System.Null_Address),
         Count  => 1,
         Active => False);
      EGL_Object'Class (Object).Post_Initialize;
   end Initialize;

   overriding procedure Adjust (Object : in out EGL_Object) is
   begin
      if Object.Reference /= null then
         Object.Reference.Count := Object.Reference.Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out EGL_Object) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => EGL_Object_Reference, Name => EGL_Object_Reference_Access);
   begin
      if Object.Reference /= null then
         Object.Reference.Count := Object.Reference.Count - 1;
         if Object.Reference.Count = 0 then
            if Object.Reference.ID /= ID_Type (System.Null_Address) then
               EGL_Object'Class (Object).Pre_Finalize;
            end if;
            Free (Object.Reference);
         end if;
      end if;

      --  Idempotence: next call to Finalize has no effect
      Object.Reference := null;
   end Finalize;

   function ID (Object : EGL_Object) return ID_Type is (Object.Reference.ID);

   function Is_Initialized (Object : EGL_Object) return Boolean is
     (Object.Reference /= null and then Object.Reference.ID /= ID_Type (System.Null_Address));

   overriding
   function "=" (Left, Right : EGL_Object) return Boolean is (Left.Reference = Right.Reference);

end EGL.Objects;
