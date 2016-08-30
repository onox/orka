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

with GL.API.Doubles;
with GL.API.Ints;
with GL.API.Shorts;
with GL.API.Singles;
with GL.API.UInts;

package body GL.Attributes is

   procedure Set_Short (Index : Attribute; Value          : Short) renames
     API.Shorts.Vertex_Attrib1;
   procedure Set_Short (Index : Attribute; V1, V2         : Short) renames
     API.Shorts.Vertex_Attrib2;
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector2) renames
     API.Shorts.Vertex_Attrib2v;
   procedure Set_Short (Index : Attribute; V1, V2, V3     : Short) renames
     API.Shorts.Vertex_Attrib3;
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector3) renames
     API.Shorts.Vertex_Attrib3v;
   procedure Set_Short (Index : Attribute; V1, V2, V3, V4 : Short) renames
     API.Shorts.Vertex_Attrib4;
   procedure Set_Short (Index : Attribute; Value          : Shorts.Vector4) renames
     API.Shorts.Vertex_Attrib4v;

   procedure Set_Single (Index : Attribute; Value          : Single) renames
     API.Singles.Vertex_Attrib1;
   procedure Set_Single (Index : Attribute; V1, V2         : Single) renames
     API.Singles.Vertex_Attrib2;
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector2) renames
     API.Singles.Vertex_Attrib2v;
   procedure Set_Single (Index : Attribute; V1, V2, V3     : Single) renames
     API.Singles.Vertex_Attrib3;
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector3) renames
     API.Singles.Vertex_Attrib3v;
   procedure Set_Single (Index : Attribute; V1, V2, V3, V4 : Single) renames
     API.Singles.Vertex_Attrib4;
   procedure Set_Single (Index : Attribute; Value          : Singles.Vector4) renames
     API.Singles.Vertex_Attrib4v;

   procedure Set_Int (Index : Attribute; Value          : Int) renames
     API.Ints.Vertex_Attrib1;
   procedure Set_Int (Index : Attribute; V1, V2         : Int) renames
     API.Ints.Vertex_Attrib2;
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector2) renames
     API.Ints.Vertex_Attrib2v;
   procedure Set_Int (Index : Attribute; V1, V2, V3     : Int) renames
     API.Ints.Vertex_Attrib3;
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector3) renames
     API.Ints.Vertex_Attrib3v;
   procedure Set_Int (Index : Attribute; V1, V2, V3, V4 : Int) renames
     API.Ints.Vertex_Attrib4;
   procedure Set_Int (Index : Attribute; Value          : Ints.Vector4) renames
     API.Ints.Vertex_Attrib4v;

   procedure Set_UInt (Index : Attribute; Value          : UInt) renames
     API.UInts.Vertex_Attrib1;
   procedure Set_UInt (Index : Attribute; V1, V2         : UInt) renames
     API.UInts.Vertex_Attrib2;
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector2) renames
     API.UInts.Vertex_Attrib2v;
   procedure Set_UInt (Index : Attribute; V1, V2, V3     : UInt) renames
     API.UInts.Vertex_Attrib3;
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector3) renames
     API.UInts.Vertex_Attrib3v;
   procedure Set_UInt (Index : Attribute; V1, V2, V3, V4 : UInt) renames
     API.UInts.Vertex_Attrib4;
   procedure Set_UInt (Index : Attribute; Value          : UInts.Vector4) renames
     API.UInts.Vertex_Attrib4v;

   procedure Set_Double (Index : Attribute; Value          : Double) renames
     API.Doubles.Vertex_Attrib1;
   procedure Set_Double (Index : Attribute; V1, V2         : Double) renames
     API.Doubles.Vertex_Attrib2;
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector2) renames
     API.Doubles.Vertex_Attrib2v;
   procedure Set_Double (Index : Attribute; V1, V2, V3     : Double) renames
     API.Doubles.Vertex_Attrib3;
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector3) renames
     API.Doubles.Vertex_Attrib3v;
   procedure Set_Double (Index : Attribute; V1, V2, V3, V4 : Double) renames
     API.Doubles.Vertex_Attrib4;
   procedure Set_Double (Index : Attribute; Value          : Doubles.Vector4) renames
     API.Doubles.Vertex_Attrib4v;

end GL.Attributes;
