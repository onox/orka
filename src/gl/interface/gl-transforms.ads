--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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

package GL.Transforms is
   pragma Preelaborate;

   use GL.Types;
   use Singles;

   procedure Translate (Matrix : in out Matrix4; Offset : Vector3);

   procedure Rotate_X (Matrix : in out Matrix4; Angle : Single);
   procedure Rotate_Y (Matrix : in out Matrix4; Angle : Single);
   procedure Rotate_Z (Matrix : in out Matrix4; Angle : Single);

   procedure Rotate (Matrix    : in out Matrix4;
                     Direction : in     Vector3;
                     Angle     : in     Single);

   procedure Scale (Matrix : in out Matrix4; Factors : Vector3);
   procedure Scale (Matrix : in out Matrix4; Factor : Single);

   function Perspective (FOV, Aspect, Z_Near, Z_Far : Single) return Matrix4;

end GL.Transforms;
