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

with Ada.Finalization;

package EGL.Objects is
   pragma Preelaborate;

   type EGL_Object is abstract new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Object : in out EGL_Object);

   overriding procedure Adjust (Object : in out EGL_Object);

   overriding procedure Finalize (Object : in out EGL_Object);

   function ID (Object : EGL_Object) return ID_Type;

   procedure Post_Initialize (Object : in out EGL_Object) is null;

   procedure Pre_Finalize (Object : in out EGL_Object) is null;

   overriding
   function "=" (Left, Right : EGL_Object) return Boolean;

private

   type EGL_Object_Reference is record
      ID    : ID_Type;
      Count : Natural;
   end record;

   type EGL_Object_Reference_Access is access all EGL_Object_Reference;

   type EGL_Object is abstract new Ada.Finalization.Controlled with record
      Reference : EGL_Object_Reference_Access;
   end record;

end EGL.Objects;
