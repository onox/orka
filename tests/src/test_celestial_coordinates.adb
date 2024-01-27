--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2024 onox <denkpadje@gmail.com>
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

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

with Orka.Celestial.Coordinates;
with Orka.Celestial.Planets;

package body Test_Celestial_Coordinates is

   use AUnit.Assertions;

   package CC renames Orka.Celestial.Coordinates;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   package Caller is new AUnit.Test_Caller (Test);

   generic
      type Element_Type is digits <>;
   function Generic_Is_Equivalent (Expected, Result : Element_Type) return Boolean;

   function Generic_Is_Equivalent (Expected, Result : Element_Type) return Boolean is
     (abs (Result - Expected) <= 1.0e-6);

   use type CC.Latitude_Degrees;
   use type CC.Longitude_Degrees;
   use type Orka.Float_64;

   function Is_Equivalent is new Generic_Is_Equivalent (CC.Latitude_Degrees);
   function Is_Equivalent is new Generic_Is_Equivalent (Orka.Float_64);

   procedure Test_From_Degrees (Object : in out Test) is
      Input : constant CC.Geodetic_Coordinate :=
         CC.From_Degrees (Orka.Celestial.Planets.Earth, Latitude => 27.9894, Longitude => 86.9255, Height => 8848.0);
   begin
      Assert (CC.Latitude (Input) = 27.9894, "Unexpected latitude");
      Assert (CC.Longitude (Input) = 86.9255, "Unexpected longitude");
      Assert (CC.Height (Input) = 8848.0, "Unexpected height");
   end Test_From_Degrees;

   procedure Test_To_Geodetic (Object : in out Test) is
      C1 : constant CC.Geodetic_Coordinate :=
         CC.From_Degrees (Orka.Celestial.Planets.Earth, Latitude => 27.9894, Longitude => 86.9255, Height => 8848.0);

      C2 : constant CC.Geocentric_Coordinate := CC.To_Geocentric (C1);
      C3 : constant CC.Geodetic_Coordinate   := CC.To_Geodetic (C2);
   begin
      Assert (Is_Equivalent (CC.Latitude (C1), CC.Latitude (C3)),
        "Unexpected latitude " & CC.Latitude (C3)'Image & " instead of " & CC.Latitude (C1)'Image);
      Assert (Is_Equivalent (CC.Height (C1), CC.Height (C3)), "Unexpected height");
   end Test_To_Geodetic;

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Celestial - Coordinates) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function From_Degrees", Test_From_Degrees'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test function To_Geodetic", Test_To_Geodetic'Access));
      return Test_Suite'Access;
   end Suite;

end Test_Celestial_Coordinates;
