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

with GL.API;
with GL.Enums.Getter;

package body GL.Shading is

   procedure Set_Minimum_Sample_Shading (Value : Normalized_Single) is
   begin
      API.Min_Sample_Shading (Value);
   end Set_Minimum_Sample_Shading;

   function Minimum_Sample_Shading return Normalized_Single is
      Result : Single := 0.0;
   begin
      API.Get_Single (Enums.Getter.Min_Sample_Shading_Value, Result);
      return Normalized_Single (Result);
   end Minimum_Sample_Shading;

end GL.Shading;
