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

with Interfaces.C;

package GL is
   pragma Preelaborate;

   package C renames Interfaces.C;

   -----------------------------------------------------------------------------
   --                                 Basics                                  --
   -----------------------------------------------------------------------------

   procedure Flush;
   procedure Finish;

   --  Index types for vectors and matrices
   type Index_Homogeneous is (X, Y, Z, W);
   subtype Index_3D is Index_Homogeneous range X .. Z;
   subtype Index_2D is Index_Homogeneous range X .. Y;

   Feature_Not_Supported_Exception : exception;
   --  Raised when a function that is not available for the current
   --  context is called

private

   -----------------------------------------------------------------------------
   --                           Internal functions                            --
   -----------------------------------------------------------------------------

   procedure Raise_Exception_On_OpenGL_Error
     with Inline;

end GL;
