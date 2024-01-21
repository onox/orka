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

package Orka.Celestial.Planets is
   pragma Preelaborate;

   AU : constant := 1.495_978_707e11;
   --  One astronomical unit (m)

   G : constant := 6.67430e-11;
   --  Gravitational constant (m^3 * kg^-1 * s^-2)

   type Physical_Characteristics is tagged record
      Axial_Tilt_Deg  : Float_64 range -180.0 .. +180.0;
      Sidereal_Day    : Duration;  -- seconds
      Flattening      : Float_64 range 0.0 .. 1.0;
      Semi_Major_Axis : Float_64;

      Mass_Kg         : Float_64;  --  kg
      Gravitational   : Float_64;  --  m^3 * s^-2
      Gravity         : Float_64;  --  m/s^2
   end record
      with Dynamic_Predicate => Flattening < 1.0 and Semi_Major_Axis > 0.0;
   --  Axial tilt (deg) and rotation (seconds) of a planet in the solar system at [1]
   --
   --  [1] https://en.wikipedia.org/wiki/Axial_tilt#Solar_System_bodies

   function Semi_Minor_Axis (Object : Physical_Characteristics) return Float_64 is
     (Object.Semi_Major_Axis * (1.0 - Object.Flattening));

   function Standard_Gravitational_Parameter (Object : Physical_Characteristics) return Float_64 is
     (G * Object.Mass_Kg);
   --  Return the standard gravitational parameter, u = G(M + m) = GM (m^3 * s^-2)

   --  Standard gravitational parameter from https://en.wikipedia.org/wiki/Standard_gravitational_parameter
   --  Axial tilt from https://en.wikipedia.org/wiki/Axial_tilt#Solar_System_bodies

   function To_Duration (Hours, Minutes : Natural := 0; Seconds, Days : Duration := 0.0) return Duration is
     (Days * 86_400.0 + Hours * 3600.0 + Minutes * 60.0 + Seconds * 1.0) with Static;

   --  Stars

   Sun : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 7.25,
      Sidereal_Day    => To_Duration (Days => 25.05),  --  At equator (34.4 days at poles)
      Flattening      => 0.0,
      Semi_Major_Axis => 696_000.0e3,
      Mass_Kg         => 1.988_409_871_327_330_5e30,
      Gravitational   => 1.327_124_400_42e20,
      Gravity         => 274.0);

   --  Planets

   Mercury : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 0.034,
      Sidereal_Day    => To_Duration (Days => 58.646),
      Flattening      => 0.000_9,
      Semi_Major_Axis => 2_439.7e3,
      Mass_Kg         => 3.3011e23,
      Gravitational   => 2.203_2e13,
      Gravity         => 3.7);

   Venus : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 177.36,  --  2.64 deg for retrograde rotation
      Sidereal_Day    => To_Duration (Days => 243.022_6),
      Flattening      => 0.0,
      Semi_Major_Axis => 6_051.8e3,
      Mass_Kg         => 4.8675e24,
      Gravitational   => 3.248_59e14,
      Gravity         => 8.87);

   --  Based on WGS 84 (See https://en.wikipedia.org/wiki/World_Geodetic_System#WGS84)
   Earth : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 23.439_2811,
      Sidereal_Day    => To_Duration (Hours => 23, Minutes => 56, Seconds => 4.0905),
      Flattening      => 1.0 / 298.257_223_563,
      Semi_Major_Axis => 6_378.137e3,
      Mass_Kg         => 5.972_168_494_074_286e24,
      Gravitational   => 3.986_004_418e14,
      Gravity         => 9.80665);

   Mars : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 25.19,
      Sidereal_Day    => To_Duration (Hours => 24, Minutes => 37, Seconds => 22.7),
      Flattening      => 0.00589,
      Semi_Major_Axis => 3_396.2e3,
      Mass_Kg         => 6.4171e23,
      Gravitational   => 4.282_837e13,
      Gravity         => 3.720_76);

   Jupiter : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 3.13,
      Sidereal_Day    => To_Duration (Hours => 9, Minutes => 55, Seconds => 30.0),
      Flattening      => 0.06487,
      Semi_Major_Axis => 71_492.0e3,
      Mass_Kg         => 1.8982e27,
      Gravitational   => 1.266_865_34e17,
      Gravity         => 24.79);

   Saturn : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 26.73,
      Sidereal_Day    => To_Duration (Hours => 10, Minutes => 33, Seconds => 38.0),
      Flattening      => 0.097_96,
      Semi_Major_Axis => 60_268.0e3,
      Mass_Kg         => 5.6834e26,
      Gravitational   => 3.793_118_7e16,
      Gravity         => 10.44);

   Uranus : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 97.77,  --  82.23 deg for retrograde
      Sidereal_Day    => To_Duration (Hours => 17, Minutes => 14, Seconds => 24.0),
      Flattening      => 0.022_9,
      Semi_Major_Axis => 25_559.0e3,
      Mass_Kg         => 8.6810e25,
      Gravitational   => 5.793_939e15,
      Gravity         => 8.69);

   Neptune : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 28.32,
      Sidereal_Day    => To_Duration (Hours => 16, Minutes => 6, Seconds => 36.0),
      Flattening      => 0.0171,
      Semi_Major_Axis => 24_764.0e3,
      Mass_Kg         => 1.024e13,
      Gravitational   => 6.836_529e15,
      Gravity         => 11.15);

   Pluto : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 122.53,   -- 57.47 deg for retrograde
      Sidereal_Day    => To_Duration (Days => 6.0, Hours => 9, Minutes => 17, Seconds => 36.0),
      Flattening      => 0.0,
      Semi_Major_Axis => 1.188_3e3,
      Mass_Kg         => 1.303e22,
      Gravitational   => 8.71e11,
      Gravity         => 0.620);

   --  Moons

   Moon : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 6.687,  --  Relative to orbit plane around Earth (1.5424 deg to ecliptic plane)
      Sidereal_Day    => To_Duration (Days => 27.321_661),
      Flattening      => 0.0012,
      Semi_Major_Axis => 1_738.1e3,
      Mass_Kg         => 7.342e22,
      Gravitational   => 4.904_869_5e12,
      Gravity         => 1.622);

   Charon : constant Physical_Characteristics :=
     (Axial_Tilt_Deg  => 0.0,  --  Tidal locked, relative to orbit plane around Pluto
      Sidereal_Day    => To_Duration (Days => 6.0, Hours => 9, Minutes => 17, Seconds => 36.0),
      Flattening      => 0.0,
      Semi_Major_Axis => 606.0e3,
      Mass_Kg         => 1.586e21,
      Gravitational   => 1.058_543_98e11,  -- Computed from Mass_Kg * G
      Gravity         => 0.288);

end Orka.Celestial.Planets;
