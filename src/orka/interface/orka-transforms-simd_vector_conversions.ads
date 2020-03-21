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

with Orka.Transforms.SIMD_Matrices;

generic
   with package From_Matrices is new Orka.Transforms.SIMD_Matrices (<>);
   with package To_Matrices   is new Orka.Transforms.SIMD_Matrices (<>);

   with function Cast
     (Elements : From_Matrices.Vector_Transforms.Vector_Type)
   return To_Matrices.Vector_Transforms.Vector_Type;
package Orka.Transforms.SIMD_Vector_Conversions is
   pragma Pure;

   function Convert
     (Elements : From_Matrices.Vector_Transforms.Vector_Type)
   return To_Matrices.Vector_Transforms.Vector_Type renames Cast;

end Orka.Transforms.SIMD_Vector_Conversions;
