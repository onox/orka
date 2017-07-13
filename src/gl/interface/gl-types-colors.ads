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

with Interfaces.C.Pointers;

package GL.Types.Colors is
   pragma Preelaborate;

   type Color_Index is (R, G, B, A);
   subtype Basic_Color_Index is Color_Index range R .. B;

   subtype Component is Single range 0.0 .. 1.0;

   type Color is array (Color_Index) of aliased Component;
   type Basic_Color is array (Basic_Color_Index) of Component;

   pragma Convention (C, Color);
   pragma Convention (C, Basic_Color);

   type Enabled_Color is array (Color_Index) of Boolean
     with Convention => C;

   type Color_Array is array (Size range <>) of aliased Color;
   type Basic_Color_Array is array (Size range <>) of aliased Basic_Color;

   package Color_Pointers is new Interfaces.C.Pointers
     (Size, Color, Color_Array, Color'(others => 0.0));
   package Basic_Color_Pointers is new Interfaces.C.Pointers
     (Size, Basic_Color, Basic_Color_Array, Basic_Color'(others => 0.0));

end GL.Types.Colors;
