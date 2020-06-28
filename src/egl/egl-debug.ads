--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with EGL.Errors;

package EGL.Debug is
   pragma Preelaborate;

   type Severity is (Critical, Error, Warning, Info);

   type Callback_Reference is access procedure
     (Error   : Errors.Error_Code;
      Level   : Severity;
      Command : String;
      Message : String);

   procedure Set_Message_Callback (Callback : Callback_Reference);

private

   for Severity use
     (Critical => 16#33B9#,
      Error    => 16#33BA#,
      Warning  => 16#33BB#,
      Info     => 16#33BC#);
   for Severity'Size use Int'Size;

end EGL.Debug;
