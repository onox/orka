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

with GL.Buffers;
with GL.Shading;
with GL.Toggles;
with GL.Types;
with GL.Viewports;

package body Orka.Contexts is

   overriding
   procedure Finalize (Object : in out Library) is
   begin
      if not Object.Finalized then
         Library'Class (Object).Shutdown;
         Object.Finalized := True;
      end if;
   end Finalize;

   overriding
   procedure Initialize (Object : in out Context) is
   begin
      Context'Class (Object).Make_Current (True);
      GL.Viewports.Set_Clipping (GL.Viewports.Lower_Left, GL.Viewports.Zero_To_One);

      Object.Vertex_Array.Create;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Context) is
   begin
      if not Object.Finalized then
         Object.Vertex_Array.Delete;

         Context'Class (Object).Make_Current (False);
         Object.Finalized := True;
      end if;
   end Finalize;

   procedure Enable (Object : in out Context; Subject : Feature) is
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
            if not Object.Enabled (Multisample) then
               raise Program_Error with "MSAA not enabled";
            end if;

            --  Enable shading per-sample. Applies if MSAA is enabled.
            --  Provides better anti-aliasing for certain cases like
            --  alpha-tested transparency
            GL.Shading.Set_Minimum_Sample_Shading (1.0);
            GL.Toggles.Enable (GL.Toggles.Sample_Shading);
      end case;
      Object.Features (Subject) := True;
   end Enable;

   function Enabled (Object : Context; Subject : Feature) return Boolean is
     (Object.Features (Subject));

end Orka.Contexts;
