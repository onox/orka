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

with AWT.Inputs;

package AWT.Clipboard is

   procedure Set (Value : String)
     with Pre => AWT.Inputs.Keyboard_Has_Focus;
   --  Write the given string to the clipboard

   type Receive_Callback is access protected procedure (Value : SU.Unbounded_String);

   procedure Get (Callback : not null Receive_Callback);
   --  Asynchronously read the content from the clipboard
   --
   --  This procedure returns immediately and the callback will be called
   --  before or after this procedure has returned.

   function Get return String;
   --  Read and return the content from the clipboard

end AWT.Clipboard;
