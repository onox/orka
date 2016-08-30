--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

package body GL.Helpers is

   function Float_Array (Value : Colors.Color) return Low_Level.Single_Array is
      use GL.Types.Colors;
   begin
      return Low_Level.Single_Array'(1 => Value (R),
                                     2 => Value (G),
                                     3 => Value (B),
                                     4 => Value (A));
   end Float_Array;

   function Color (Value : Low_Level.Single_Array) return Colors.Color is
      use GL.Types.Colors;
   begin
      return Colors.Color'(R => Value (1), G => Value (2), B => Value (3),
                           A => Value (4));
   end Color;

end GL.Helpers;
