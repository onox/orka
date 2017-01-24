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

with Ada.Containers.Indefinite_Holders;

package GL.Debug.Logs is
   pragma Preelaborate;

   package String_Holder is new Ada.Containers.Indefinite_Holders
     (Element_Type => String);

   type Message is record
      From      : Source;
      Kind      : Message_Type;
      Level     : Severity;
      ID        : UInt;
      Message   : String_Holder.Holder;
   end record;

   type Message_Array is array (Size range <>) of Message;

   function Message_Log return Message_Array;
   --  Return an array containing the debug messages that are in the log
   --
   --  After having called this function, the messages that were returned
   --  in the array are removed from the log.

   function Logged_Messages return Size;

end GL.Debug.Logs;
