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
   with function "/" (Left, Right : Element_Type) return Element_Type is <>;
package GL.Vectors is
   pragma Preelaborate;
   
   type Vector is array (Index_Type) of aliased Element_Type;
   pragma Convention (C, Vector);
   
   function "+" (Left, Right : Vector) return Vector;
   
   function "-" (Left, Right : Vector) return Vector;
   function "-" (Left        : Vector) return Vector;
   
   function "*" (Left : Vector;       Right : Element_Type) return Vector;
   function "*" (Left : Element_Type; Right : Vector)       return Vector;
   
   function "/" (Left : Vector; Right : Element_Type) return Vector;
   
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");
end GL.Vectors;
