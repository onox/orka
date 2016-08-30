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

package body GL.Algebra is
   
   function To_Vector2 (Vector : Vector3) return Vector2 is
   begin
      return Vector2'(Vector (X), Vector (Y));
   end To_Vector2;
   
   function To_Vector2 (Vector : Vector4) return Vector2 is
   begin
      return Vector2'(Vector (X), Vector (Y));
   end To_Vector2;
   
   function To_Vector3 (Vector : Vector2) return Vector3 is
   begin
      return Vector3'(Vector (X), Vector (Y), Null_Value);
   end To_Vector3;
   
   function To_Vector3 (Vector : Vector4) return Vector3 is
   begin
      return Vector3'(Vector (X), Vector (Y), Vector (Z));
   end To_Vector3;
   
   function To_Vector4 (Vector : Vector2) return Vector4 is
   begin
      return Vector4'(Vector (X), Vector (Y), Null_Value, One_Value);
   end To_Vector4;
   
   function To_Vector4 (Vector : Vector3) return Vector4 is
   begin
      return Vector4'(Vector (X), Vector (Y), Vector (Z), One_Value);
   end To_Vector4;
   
end GL.Algebra;
