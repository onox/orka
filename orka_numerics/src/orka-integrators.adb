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

package body Orka.Integrators is

   function RK4
     (Y  : Value;
      DT : Duration_Type;
      F  : not null access function (Y : Value; DT : Duration_Type) return Derivative)
   return Derivative is
      A, B, C, D : Derivative;
   begin
      A := DT * F (Y,           DT * 0.0);
      B := DT * F (Y + 0.5 * A, DT * 0.5);
      C := DT * F (Y + 0.5 * B, DT * 0.5);
      D := DT * F (Y + C,       DT * 1.0);

      return (1.0 / 6.0) * (A + 2.0 * (B + C) + D);
   end RK4;

end Orka.Integrators;
