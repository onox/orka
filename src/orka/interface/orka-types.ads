--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with GL.Types;

package Orka.Types is
   pragma Preelaborate;

   type Non_Numeric_Type is
     (Single_Vector_Type,
      Double_Vector_Type,
      Single_Matrix_Type,
      Double_Matrix_Type,
      Arrays_Command_Type,
      Elements_Command_Type);

   function Convert (Elements : GL.Types.Single_Array) return GL.Types.Half_Array;
   function Convert (Elements : GL.Types.Half_Array) return GL.Types.Single_Array;

end Orka.Types;
