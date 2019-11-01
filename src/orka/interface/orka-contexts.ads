--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

private with Ada.Finalization;

package Orka.Contexts is
   pragma Preelaborate;

   type Feature is (Reversed_Z, Multisample, Sample_Shading);

   type Context (Debug : Boolean) is tagged limited private;

   procedure Shutdown (Object : in out Context) is null;

   procedure Enable (Object : in out Context; Subject : Feature);
   --  Note: If enabling Reversed_Z, the depth must be cleared with the
   --  value 0.0

   function Enabled (Object : Context; Subject : Feature) return Boolean;

private

   type Feature_Array is array (Feature) of Boolean;

   type Context (Debug : Boolean) is limited new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean := False;
      Features  : Feature_Array := (others => False);
   end record;

   overriding
   procedure Initialize (Object : in out Context);

   overriding
   procedure Finalize (Object : in out Context);

end Orka.Contexts;
