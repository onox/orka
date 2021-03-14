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

private package Orka.Strings is
   pragma Preelaborate;

   function Trim (Value : String) return String;
   --  Return value with whitespace removed from both the start and end

   function Strip_Line_Term (Value : String) return String;
   --  Return the value without any LF and CR characters at the end

   function Lines (Value : String) return Positive;
   --  Return the number of lines that the string contains

end Orka.Strings;
