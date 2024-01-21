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

with Ada.Numerics;

with Orka.Celestial.Planets;
with Orka.Transforms.Doubles.Vectors;

package Orka.Celestial.Coordinates is
   pragma Preelaborate;

   package Vectors renames Orka.Transforms.Doubles.Vectors;

   type Longitude_Degrees is new Float_64 range -180.0 .. +180.0;
   type Latitude_Degrees  is new Float_64 range  -90.0 .. +90.0;

   type Geodetic_Coordinate is private;

   function From_Degrees
     (Planet    : aliased Planets.Physical_Characteristics;
      Latitude  : Latitude_Degrees;
      Longitude : Longitude_Degrees;
      Height    : Float_64) return Geodetic_Coordinate;
   --  Return a geodetic coordinate of the position given by its latitude, longitude, and height
   --  above the ellipsoid of the planet

   type Geocentric_Coordinate is private;
   --  Geocentric coordinates or Earth-Centered Earth-Fixed (ECEF)
   --
   --  The reference frame rotates with the planet.
   --
   --  The origin is the center of mass of the planet. The z-axis intersects
   --  the true north (north pole), while the x-axis intersects the planet
   --  at 0 deg latitude and 0 deg longitude.

   function To_Geocentric (Value : Geodetic_Coordinate) return Geocentric_Coordinate;
   --  Return the geocentric coordinate of the position given by its geodetic coordinate

   function To_Geodetic (Value : Geocentric_Coordinate) return Geodetic_Coordinate;

   type Geocentric_Inertial_Coordinate is private;
   --  Geocentric inertial coordinates or Earth-Centered Inertial (ECI)
   --
   --  The reference frame does *not* rotate with the planet.
   --
   --  The origin is the center of mass of the planet. The x axis points
   --  towards the vernal equinox. The z axis is aligned with the planet's
   --  axis of rotation as it intersects the true north (north pole) of the planet.

   function To_Geocentric_Inertial
     (Value   : Geocentric_Coordinate;
      Seconds : Duration) return Geocentric_Inertial_Coordinate;
   --  Return the geocentric inertial coordinate of the given geocentric coordinate at
   --  the time given by the number of seconds since the epoch.

   function To_Geocentric (Value : Geocentric_Inertial_Coordinate) return Geocentric_Coordinate;

   type Geocentric_Ecliptic_Coordinate is private;
   --  Geocentric ecliptic coordinates
   --
   --  The origin is the center of mass of the planet. The x axis points
   --  towards the vernal equinox. The z axis is perpendicular to the ecliptic plane
   --  (the orbital plane of the planet around its star).
   --
   --  Note: the reference frame does not take into account any precession or nutation.

   function To_Geocentric_Ecliptic (Value : Geocentric_Inertial_Coordinate) return Geocentric_Ecliptic_Coordinate;
   function To_Geocentric_Inertial (Value : Geocentric_Ecliptic_Coordinate) return Geocentric_Inertial_Coordinate;

   -----------------------------------------------------------------------------

   type Local_Tangent_Plane is private;

   function To_Local_Tangent_Plane
     (Value     : Geocentric_Coordinate;
      Reference : Geodetic_Coordinate) return Local_Tangent_Plane;

   function To_Geocentric (Value : Local_Tangent_Plane) return Geocentric_Coordinate;

   -----------------------------------------------------------------------------

   function To_Vector (Value : Geocentric_Coordinate) return Vectors.Vector4;
   function To_Vector (Value : Geocentric_Inertial_Coordinate) return Vectors.Vector4;
   function To_Vector (Value : Geocentric_Ecliptic_Coordinate) return Vectors.Vector4;

   function To_3D (Position : Vectors.Vector4) return Vectors.Vector4;

private

   type Latitude_Radians  is new Float_64 range  -0.5 * Ada.Numerics.Pi .. +0.5 * Ada.Numerics.Pi;
   type Longitude_Radians is new Float_64 range -Ada.Numerics.Pi .. +Ada.Numerics.Pi;

   type Geodetic_Coordinate is record
      Planet    : not null access constant Planets.Physical_Characteristics;
      Latitude  : Latitude_Radians;
      Longitude : Longitude_Radians;
      Height    : Float_64;
   end record;

   type Geocentric_Coordinate is record
      Planet   : not null access constant Planets.Physical_Characteristics;
      Position : Vectors.Vector4;
   end record;

   type Geocentric_Inertial_Coordinate is record
      Planet   : not null access constant Planets.Physical_Characteristics;
      Seconds  : Duration;
      Position : Vectors.Vector4;
   end record;

   type Geocentric_Ecliptic_Coordinate is record
      Planet   : not null access constant Planets.Physical_Characteristics;
      Seconds  : Duration;
      Position : Vectors.Vector4;
   end record;

   type Local_Tangent_Plane is record
      Planet    : not null access constant Planets.Physical_Characteristics;
      Local     : Vectors.Vector4;
      Reference : Geodetic_Coordinate;
   end record;

end Orka.Celestial.Coordinates;
