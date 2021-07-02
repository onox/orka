--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

package Orka.Atomics is
   pragma Pure;

   protected type Counter (Initial_Value : Natural) is
      pragma Lock_Free;

      procedure Add (Addition : Natural);

      procedure Increment;

      procedure Decrement (Zero : out Boolean);

      procedure Reset;

      function Count return Natural;
   private
      Value : Natural := Initial_Value;
   end Counter;

end Orka.Atomics;
