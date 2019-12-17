--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with Orka.Transforms.SIMD_Vector_Conversions;

generic
   with package Conversions is new Orka.Transforms.SIMD_Vector_Conversions (<>);
package Orka.Transforms.SIMD_Matrix_Conversions is
   pragma Preelaborate;

   function Convert
     (Elements : Conversions.From_Matrices.Matrix_Type) return Conversions.To_Matrices.Matrix_Type
   is (Conversions.Convert (Elements (X)),
       Conversions.Convert (Elements (Y)),
       Conversions.Convert (Elements (Z)),
       Conversions.Convert (Elements (W)));

end Orka.Transforms.SIMD_Matrix_Conversions;
