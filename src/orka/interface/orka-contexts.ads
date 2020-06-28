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

   -----------------------------------------------------------------------------

   type Feature is (Reversed_Z, Multisample, Sample_Shading);

   procedure Enable (Object : in out Context; Subject : Feature) is abstract;
   --  Note: If enabling Reversed_Z, the depth must be cleared with the
   --  value 0.0

   function Enabled (Object : Context; Subject : Feature) return Boolean is abstract;

   -----------------------------------------------------------------------------
   --                             Helper utilities                            --
   -----------------------------------------------------------------------------

   type Feature_Array is array (Feature) of Boolean;

   procedure Enable (Features : in out Feature_Array; Subject : Feature);

   function Enabled (Features : Feature_Array; Subject : Feature) return Boolean;

   -----------------------------------------------------------------------------
   --                         Contexts with a surface                         --
   -----------------------------------------------------------------------------

   type Surface_Context is limited interface and Context;

   function Create_Window
     (Object             : Surface_Context;
      Width, Height      : Positive;
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True) return Orka.Windows.Window'Class is abstract;

end Orka.Contexts;
