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

with Ada.Numerics.Generic_Elementary_Functions;

with Orka.Transforms.Doubles.Matrices;

package body Orka.Celestial.Coordinates is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);

   package Matrices renames Orka.Transforms.Doubles.Matrices;

   function From_Degrees
     (Planet    : aliased Planets.Physical_Characteristics;
      Latitude  : Latitude_Degrees;
      Longitude : Longitude_Degrees;
      Height    : Float_64) return Geodetic_Coordinate
   is (Planet    => Planet'Access,
       Latitude  => Latitude_Radians (Vectors.To_Radians (Float_64 (Latitude))),
       Longitude => Longitude_Radians (Vectors.To_Radians (Float_64 (Longitude))),
       Height    => Height);

   function Get_Vector
     (Value : Geodetic_Coordinate) return Vectors.Vector4
   is
      XY : constant Float_64 := EF.Cos (Float_64 (Value.Latitude));

      X : constant Float_64 := XY * EF.Cos (Float_64 (Value.Longitude));
      Y : constant Float_64 := XY * EF.Sin (Float_64 (Value.Longitude));
      Z : constant Float_64 := EF.Sin (Float_64 (Value.Latitude));
   begin
      pragma Assert (Vectors.Normalized ([X, Y, Z, 0.0]));
      return [X, Y, Z, 1.0];
   end Get_Vector;

   function Flattened_Vector
     (Planet    : Planets.Physical_Characteristics;
      Direction : Vectors.Vector4;
      Altitude  : Float_64) return Vectors.Vector4
   is
      E2 : constant Float_64 := 2.0 * Planet.Flattening - Planet.Flattening ** 2;

      N : constant Float_64 := Planet.Semi_Major_Axis /
        EF.Sqrt (1.0 - E2 * Direction (Z) ** 2);
   begin
      return
        [Direction (X) * (N + Altitude),
         Direction (Y) * (N + Altitude),
         Direction (Z) * (N * (1.0 - E2) + Altitude),
         1.0];
   end Flattened_Vector;

   function To_Geocentric (Value : Geodetic_Coordinate) return Geocentric_Coordinate is
     ((Planet   => Value.Planet,
       Position => Flattened_Vector (Value.Planet.all, Get_Vector (Value), Value.Height)));

   function To_Geodetic (Value : Geocentric_Coordinate) return Geodetic_Coordinate is
      use EF;

      --  Closed-form computation of the geodetic coordinate by computing the quartic equation of K

      E2 : constant Float_64 := 2.0 * Value.Planet.Flattening - Value.Planet.Flattening ** 2;
      E4 : constant Float_64 := E2 * E2;
      A2 : constant Float_64 := Value.Planet.Semi_Major_Axis ** 2;

      P : constant Float_64 := EF.Sqrt (Value.Position (X) ** 2 + Value.Position (Y) ** 2);

      Zeta : constant Float_64 := (1.0 - E2) * (Value.Position (Z) ** 2) / A2;

      Rho : constant Float_64 := (1.0 / 6.0) * ((P ** 2) / A2 + Zeta - E4);

      S : constant Float_64 := (E4 * Zeta * (P ** 2)) / (4.0 * (Rho ** 3) * A2);
      T : constant Float_64 := (1.0 + S + EF.Sqrt (S * (S + 2.0))) ** (1.0 / 3.0);

      U : constant Float_64 := Rho * (T + 1.0 + 1.0 / T);
      V : constant Float_64 := EF.Sqrt (U ** 2 + E4 * Zeta);
      W : constant Float_64 := E2 * (U + V - Zeta) / (2.0 * V);

      K : constant Float_64 := 1.0 + E2 * (EF.Sqrt (U + V + W ** 2) + W) / (U + V);

      ----------------------------------------------------------------------

      Longitude : constant Float_64 := EF.Arctan (Value.Position (Y), Value.Position (X));
      Latitude  : constant Float_64 := EF.Arctan (K * (Value.Position (Z) / P));

      N : constant Float_64 := Value.Planet.Semi_Major_Axis /
        EF.Sqrt (1.0 - E2 * EF.Sin (Latitude) ** 2);

      H : constant Float_64 := P / EF.Cos (Latitude) - N;
   begin
      return
        (Planet    => Value.Planet,
         Latitude  => Latitude_Radians (Latitude),
         Longitude => Longitude_Radians (Longitude),
         Height    => H);
   end To_Geodetic;

   use Matrices;

   function Angular (Seconds, Sidereal : Duration) return Float_64 is
     (Float_64 (Seconds / Sidereal) * 2.0 * Ada.Numerics.Pi);

   function To_Geocentric_Inertial
     (Value   : Geocentric_Coordinate;
      Seconds : Duration) return Geocentric_Inertial_Coordinate
   is
     (Planet   => Value.Planet,
      Seconds  => Seconds,
      Position => Rz (Angular (Seconds, Value.Planet.Sidereal_Day)) * Value.Position);

   function To_Geocentric (Value : Geocentric_Inertial_Coordinate) return Geocentric_Coordinate is
     (Planet   => Value.Planet,
      Position => Rz (Angular (-Value.Seconds, Value.Planet.Sidereal_Day)) * Value.Position);

   function To_Geocentric_Ecliptic (Value : Geocentric_Inertial_Coordinate) return Geocentric_Ecliptic_Coordinate is
     (Planet   => Value.Planet,
      Seconds  => Value.Seconds,
      Position => Rx (-Vectors.To_Radians (Value.Planet.Axial_Tilt_Deg)) * Value.Position);

   function To_Geocentric_Inertial (Value : Geocentric_Ecliptic_Coordinate) return Geocentric_Inertial_Coordinate is
     (Planet   => Value.Planet,
      Seconds  => Value.Seconds,
      Position => Rx (Vectors.To_Radians (Value.Planet.Axial_Tilt_Deg)) * Value.Position);

   -----------------------------------------------------------------------------

   function To_Local_Tangent_Plane
     (Value     : Geocentric_Coordinate;
      Reference : Geodetic_Coordinate) return Local_Tangent_Plane
   is
      Cos_Lon : constant Float_64 := EF.Cos (Float_64 (Reference.Longitude));
      Sin_Lon : constant Float_64 := EF.Sin (Float_64 (Reference.Longitude));

      Cos_Lat : constant Float_64 := EF.Cos (Float_64 (Reference.Latitude));
      Sin_Lat : constant Float_64 := EF.Sin (Float_64 (Reference.Latitude));

      use Vectors;

      Transform_To_ENU : Matrix4 := Identity_Matrix;
   begin
      Transform_To_ENU (X) := [-Sin_Lon, -Sin_Lat * Cos_Lon, Cos_Lat * Cos_Lon, 0.0];
      Transform_To_ENU (Y) := [Cos_Lon, -Sin_Lat * Sin_Lon, Cos_Lat * Sin_Lon, 0.0];
      Transform_To_ENU (Z) := [0.0,  Cos_Lat, Sin_Lat, 0.0];

      return
        (Planet    => Value.Planet,
         Local     => Transform_To_ENU * (Value.Position - To_Geocentric (Reference).Position),
         Reference => Reference);
   end To_Local_Tangent_Plane;

   function To_Geocentric (Value : Local_Tangent_Plane) return Geocentric_Coordinate is
      Cos_Lon : constant Float_64 := EF.Cos (Float_64 (Value.Reference.Longitude));
      Sin_Lon : constant Float_64 := EF.Sin (Float_64 (Value.Reference.Longitude));

      Cos_Lat : constant Float_64 := EF.Cos (Float_64 (Value.Reference.Latitude));
      Sin_Lat : constant Float_64 := EF.Sin (Float_64 (Value.Reference.Latitude));

      use Vectors;

      Transform_To_ECEF : Matrix4 := Identity_Matrix;
   begin
      Transform_To_ECEF (X) := [-Sin_Lon, Cos_Lon, 0.0, 0.0];
      Transform_To_ECEF (Y) := [-Sin_Lat * Cos_Lon, -Sin_Lat * Sin_Lon, Cos_Lat, 0.0];
      Transform_To_ECEF (Z) := [Cos_Lat * Cos_Lon, Cos_Lat * Sin_Lon, Sin_Lat, 0.0];

      return
        (Planet   => Value.Planet,
         Position => (Transform_To_ECEF * Value.Local) + To_Geocentric (Value.Reference).Position);
   end To_Geocentric;

   -----------------------------------------------------------------------------

   function To_Vector (Value : Geocentric_Coordinate) return Vectors.Vector4 is (Value.Position);
   function To_Vector (Value : Geocentric_Inertial_Coordinate) return Vectors.Vector4 is (Value.Position);
   function To_Vector (Value : Geocentric_Ecliptic_Coordinate) return Vectors.Vector4 is (Value.Position);

   function To_3D (Position : Vectors.Vector4) return Vectors.Vector4 is
     ([Position (Y), Position (Z), Position (X), 1.0]);

end Orka.Celestial.Coordinates;
