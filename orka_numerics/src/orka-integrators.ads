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

package Orka.Integrators is
   pragma Preelaborate;

   generic
      type Value      is private;
      type Derivative is private;

      type Duration_Type is digits <>;

      with function "*" (Left : Duration_Type; Right : Derivative) return Derivative is <>;
      with function "+" (Left : Value;         Right : Derivative) return Value      is <>;
      with function "+" (Left : Derivative;    Right : Derivative) return Derivative is <>;
   function RK4
     (Y  : Value;
      DT : Duration_Type;
      F  : not null access function (Y : Value; DT : Duration_Type) return Derivative)
   return Derivative;
   --  Return the change to Y for the given DT time step using
   --  the Runge-Kutta 4th order method
   --
   --  If F represent a time-variant system, that is, F depends on a time T,
   --  then you must compute the derivative at time T + DT. In this case you
   --  must keep track of T yourself and add it to the given DT.
   --
   --  The returned value must be added to Y to perform the numerical integration.
   --
   --  For example, to numerically integrate a position 10 times per second:
   --
   --     X := X + RK4 (X, 0.1, DX'Access);

end Orka.Integrators;
