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

with GL.Low_Level;
with GL.Types.Colors;

private package GL.Helpers is
   pragma Preelaborate;
   
   use GL.Types;

   function Float_Array (Value : Colors.Color) return Low_Level.Single_Array;
   function Color (Value : Low_Level.Single_Array) return Colors.Color;

end GL.Helpers;
