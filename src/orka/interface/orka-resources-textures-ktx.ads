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

with Ada.Real_Time;

with Orka.Jobs;

package Orka.Resources.Textures.KTX is
--   pragma Preelaborate;

   procedure Load
     (Bytes   : in out Byte_Array_Access;
      Time    : Ada.Real_Time.Time_Span;
      Path    : SU.Unbounded_String;
      Enqueue : not null access procedure (Element : Jobs.Job_Ptr));

   function Load_Texture (Path : String) return Texture;

end Orka.Resources.Textures.KTX;
