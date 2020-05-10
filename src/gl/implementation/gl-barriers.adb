--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Unchecked_Conversion;

with GL.API;

package body GL.Barriers is

   procedure Texture_Barrier is
   begin
      API.Texture_Barrier.Ref.all;
   end Texture_Barrier;

   procedure Memory_Barrier (Bits : Memory_Barrier_Bits) is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Memory_Barrier_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Bits) and 2#1111111111101111#;
   begin
      API.Memory_Barrier.Ref (Raw_Bits);
   end Memory_Barrier;

   procedure Memory_Barrier_By_Region (Bits : Memory_Barrier_Bits) is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Memory_Barrier_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Bits) and 2#0011010000101100#;
   begin
      API.Memory_Barrier_By_Region.Ref (Raw_Bits);
   end Memory_Barrier_By_Region;

end GL.Barriers;
