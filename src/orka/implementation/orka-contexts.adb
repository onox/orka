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

with Ada.Strings.Unbounded;

with GL.Buffers;
with GL.Shading;
with GL.Toggles;
with GL.Types;

with Orka.Strings;

package body Orka.Contexts is

   function Image (Version : Context_Version) return String is
     (Strings.Trim (Version.Major'Image) & "." & Strings.Trim (Version.Minor'Image));

   function Image (Flags : Context_Flags) return String is
      package SU renames Ada.Strings.Unbounded;

      Result : SU.Unbounded_String;
   begin
      if Flags.Debug then
         SU.Append (Result, " debug");
      end if;
      if Flags.Robust then
         SU.Append (Result, " robust");
      end if;
      if Flags.No_Error then
         SU.Append (Result, " no-error");
      end if;
      return Strings.Trim (SU.To_String (Result));
   end Image;

   procedure Enable (Features : in out Feature_Array; Subject : Feature) is
   begin
      case Subject is
         when Reversed_Z =>
            --  Enable reversed Z for better depth precision at great distances
            --  See https://developer.nvidia.com/content/depth-precision-visualized
            --  for a visualization
            GL.Buffers.Set_Depth_Function (GL.Types.Greater);
            --  When clearing the depth buffer, the value 0.0 instead of 1.0 must be used
         when Multisample =>
            --  Enable MSAA
            GL.Toggles.Enable (GL.Toggles.Multisample);
         when Sample_Shading =>
            if not Enabled (Features, Multisample) then
               raise Program_Error with "MSAA not enabled";
            end if;

            --  Enable shading per-sample. Applies if MSAA is enabled.
            --  Provides better anti-aliasing for certain cases like
            --  alpha-tested transparency
            GL.Shading.Set_Minimum_Sample_Shading (1.0);
            GL.Toggles.Enable (GL.Toggles.Sample_Shading);
      end case;
      Features (Subject) := True;
   end Enable;

   function Enabled (Features : Feature_Array; Subject : Feature) return Boolean is
     (Features (Subject));

end Orka.Contexts;
