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

with GL.Types;

package GL.Shading is
   pragma Preelaborate;

   use GL.Types;

   procedure Set_Minimum_Sample_Shading (Value : Normalized_Single);
   --  Set the minimum amount of samples for which the fragment
   --  shader should run.
   --
   --  The default fraction is 0.0 with a minimum of 1 sample. Sample
   --  shading can be used while multisampling is enabled.
   --
   --  For example, if you use MSAA 8x and set Value to 0.5, then the
   --  fragment shader will be run for at least 4 samples per pixel.

   function Minimum_Sample_Shading return Normalized_Single;
   --  Return the current fraction of samples that are shaded

end GL.Shading;
