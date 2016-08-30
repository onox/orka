--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

package GL.Attributes is
   pragma Preelaborate;

   use GL.Types;

   type Attribute is new UInt;

   procedure Set_Short (Index : Attribute; Value          : Short);
   procedure Set_Short (Index : Attribute; V1, V2         : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector2);
   procedure Set_Short (Index : Attribute; V1, V2, V3     : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector3);
   procedure Set_Short (Index : Attribute; V1, V2, V3, V4 : Short);
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector4);

   procedure Set_Single (Index : Attribute; Value          : Single);
   procedure Set_Single (Index : Attribute; V1, V2         : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector2);
   procedure Set_Single (Index : Attribute; V1, V2, V3     : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector3);
   procedure Set_Single (Index : Attribute; V1, V2, V3, V4 : Single);
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector4);

   procedure Set_Int (Index : Attribute; Value          : Int);
   procedure Set_Int (Index : Attribute; V1, V2         : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector2);
   procedure Set_Int (Index : Attribute; V1, V2, V3     : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector3);
   procedure Set_Int (Index : Attribute; V1, V2, V3, V4 : Int);
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector4);

   procedure Set_UInt (Index : Attribute; Value          : UInt);
   procedure Set_UInt (Index : Attribute; V1, V2         : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector2);
   procedure Set_UInt (Index : Attribute; V1, V2, V3     : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector3);
   procedure Set_UInt (Index : Attribute; V1, V2, V3, V4 : UInt);
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector4);

   procedure Set_Double (Index : Attribute; Value          : Double);
   procedure Set_Double (Index : Attribute; V1, V2         : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector2);
   procedure Set_Double (Index : Attribute; V1, V2, V3     : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector3);
   procedure Set_Double (Index : Attribute; V1, V2, V3, V4 : Double);
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector4);

end GL.Attributes;
