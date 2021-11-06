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

package body Orka.Numerics.Tensors is

   function Elements (Shape : Tensor_Shape) return Natural is
      Result : Natural := 1;
   begin
      for Elements of Shape loop
         Result := Result * Elements;
      end loop;

      return Result;
   end Elements;

   function Add (Left, Right : Tensor_Shape; Dimension : Tensor_Dimension) return Tensor_Shape is
      Result : Tensor_Shape := Left;
   begin
      Result (Dimension) := Result (Dimension) + Right (Dimension);

      return Result;
   end Add;

   function Trim (Value : Natural) return String is
      Result : constant String := Value'Image;
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Trim;

   function Image (Shape : Tensor_Shape) return String is
     (case Shape'Length is
        when 1 =>
           "(" & Trim (Shape (1)) & ")",
        when 2 =>
           "(" & Trim (Shape (1)) & ", " & Trim (Shape (2)) & ")",
        when others =>
           raise Program_Error);

end Orka.Numerics.Tensors;
