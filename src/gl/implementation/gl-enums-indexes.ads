--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Types;

generic
   Min_Representation : Types.Int;
   Maximum : GL.Types.Int;
package GL.Enums.Indexes is
   pragma Preelaborate;

   use GL.Types;

   subtype Index is Int range 0 .. Maximum;

   function Representation (Value : Index) return Int;

   function Value (Representation : Int) return Index;

end GL.Enums.Indexes;
