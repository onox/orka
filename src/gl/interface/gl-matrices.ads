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

generic
   type Index_Type is (<>);
   type Element_Type is private;
   with function "+" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left, Right : Element_Type) return Element_Type is <>;
   with function "-" (Left        : Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : Element_Type) return Element_Type is <>;
   -- not needed currently
   --with function "/" (Left, Right : Element_Type) return Element_Type is <>;
   type Vector_Type is array (Index_Type) of aliased Element_Type;
   Null_Value : Element_Type;
package GL.Matrices is
   pragma Preelaborate;
   
   type Matrix is array (Index_Type, Index_Type) of aliased Element_Type;
   
   function "+" (Left, Right : Matrix) return Matrix;
   function "-" (Left, Right : Matrix) return Matrix;
   function "-" (Left : Matrix) return Matrix;
   
   -- This is not element-wise but mathematical matrix multiplication.
   function "*" (Left, Right : Matrix) return Matrix;
   
   function "*" (Left : Matrix; Right : Vector_Type) return Vector_Type;
   
   function "*" (Left : Matrix; Right : Element_Type) return Matrix;
   function "*" (Left : Element_Type; Right : Matrix) return Matrix;
   
   function Transpose (Subject : Matrix) return Matrix;
   
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline (Transpose);
   
   pragma Convention (C, Matrix);
end GL.Matrices;
