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

with Interfaces.C.Pointers;

package Orka.Types.Pointers is
   pragma Preelaborate;

   package Single_Vector4_Pointers is new Interfaces.C.Pointers
     (Size, Singles.Vector4, Singles.Vector4_Array,
      Singles.Vector4'(others => 0.0));

   package Single_Matrix4_Pointers is new Interfaces.C.Pointers
     (Size, Singles.Matrix4, Singles.Matrix4_Array,
      Singles.Matrix4'(others => (others => 0.0)));

   package Double_Vector4_Pointers is new Interfaces.C.Pointers
     (Size, Doubles.Vector4, Doubles.Vector4_Array,
      Doubles.Vector4'(others => 0.0));

   package Double_Matrix4_Pointers is new Interfaces.C.Pointers
     (Size, Doubles.Matrix4, Doubles.Matrix4_Array,
      Doubles.Matrix4'(others => (others => 0.0)));

end Orka.Types.Pointers;
