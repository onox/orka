--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with Orka.Windows;

package Orka.Contexts is
   pragma Preelaborate;

   type Context_Version is record
      Major, Minor : Natural;
   end record
     with Dynamic_Predicate => Context_Version.Major >= 4
            or else (Context_Version.Major = 3 and Context_Version.Minor >= 2);

   type Context_Flags is record
      Debug    : Boolean := False;
      Robust   : Boolean := False;
      No_Error : Boolean := False;
   end record
     with Dynamic_Predicate => (if Context_Flags.No_Error then
                                  not (Context_Flags.Debug or Context_Flags.Robust));

   function Image (Version : Context_Version) return String;

   function Image (Flags : Context_Flags) return String;

   type Context is limited interface;

   function Create_Context
     (Version : Context_Version;
      Flags   : Context_Flags := (others => False)) return Context is abstract;

   function Version (Object : Context) return Context_Version is abstract;

   function Flags (Object : Context) return Context_Flags is abstract;

   type Task_Kind is (Current_Task, Any_Task);

   function Is_Current (Object : Context; Kind : Task_Kind) return Boolean is abstract;

   procedure Make_Current (Object : Context) is abstract
     with Pre'Class  => not Object.Is_Current (Any_Task),
          Post'Class =>     Object.Is_Current (Current_Task);

   procedure Make_Not_Current (Object : Context) is abstract
     with Pre'Class  =>     Object.Is_Current (Current_Task),
          Post'Class => not Object.Is_Current (Any_Task);

   -----------------------------------------------------------------------------
   --                         Contexts with a surface                         --
   -----------------------------------------------------------------------------

   type Surface_Context is limited interface and Context;

   procedure Make_Current
     (Object : Surface_Context;
      Window : in out Orka.Windows.Window'Class) is abstract
   with Pre'Class  => Object.Is_Current (Current_Task) or not Object.Is_Current (Any_Task),
        Post'Class => Object.Is_Current (Current_Task);

end Orka.Contexts;
