--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Low_Level.Enums;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Types.Compute;

limited with GL.Objects.Programs.Uniforms;

package GL.Objects.Programs is
   pragma Preelaborate;

   type Program is new GL_Object with private;

   procedure Attach (Subject : Program; Shader : Shaders.Shader);

   procedure Detach (Subject : Program; Shader : Shaders.Shader);

   procedure Link (Subject : Program);

   function Link_Status (Subject : Program) return Boolean;

   function Info_Log (Subject : Program) return String;

   procedure Use_Program (Subject : Program);
   --  Use the shaders of the given program during rendering
   --
   --  If you have subroutines in some of its shaders, you must
   --  subsequently call Set_Uniform_Subroutines, because the subroutine
   --  state is completely lost after having called Use_Program.

   procedure Set_Separable (Subject : Program; Separable : Boolean);
   function Separable (Subject : Program) return Boolean;

   function Compute_Work_Group_Size (Object : Program) return Compute.Dimension_Size_Array;
   --  Size (per dimension) of a local work group in the linked compute stage

   function Uniform_Location (Subject : Program; Name : String)
     return Programs.Uniforms.Uniform;
   --  Return a Uniform given its name
   --
   --  The returned object can be used to set values for use in the
   --  shaders.
   --
   --  Raises the Uniform_Inactive_Error exception if the name
   --  does not exist or is unused.

   function Uniform_Type (Object : Program; Name : String)
     return Low_Level.Enums.Resource_Type;
   --  Raises the Uniform_Inactive_Error exception if the name
   --  does not exist or is unused.

   function Buffer_Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Size;

   overriding
   procedure Initialize_Id (Object : in out Program);

   procedure Initialize_Id (Object : in out Program; Kind : Shaders.Shader_Type; Source : String);

   overriding
   procedure Delete_Id (Object : in out Program);

   overriding
   function Identifier (Object : Program) return Types.Debug.Identifier is
     (Types.Debug.Program);

   Uniform_Inactive_Error    : exception;
   Subroutine_Inactive_Error : exception;

   -----------------------------------------------------------------------------
   --                               Subroutines                               --
   -----------------------------------------------------------------------------

   subtype Subroutine_Index_Type is UInt;
   subtype Uniform_Location_Type is Int range -1 .. Int'Last;

   type Subroutine_Index_Array is array (Size range <>) of Subroutine_Index_Type;

   Invalid_Index : constant Subroutine_Index_Type;

   function Subroutine_Index
     (Object : Program;
      Shader : Shaders.Shader_Type;
      Name   : String) return Subroutine_Index_Type;
   --  Return the index of the subroutine function given its name
   --
   --  Raises the Subroutine_Inactive_Error exception if the name
   --  does not exist or is unused.

   function Subroutine_Uniform_Index
     (Object : Program;
      Shader : Shaders.Shader_Type;
      Name   : String) return Subroutine_Index_Type;
   --  Return the index of the subroutine uniform given its name
   --
   --  Raises the Uniform_Inactive_Error exception if the name
   --  does not exist or is unused.

   function Subroutine_Uniform_Location
     (Object : Program;
      Shader : Shaders.Shader_Type;
      Name   : String) return Uniform_Location_Type;
   --  Return the location of a subroutine uniform
   --
   --  The location of the uniform is used when setting the subroutine
   --  function (using the index of the function) that should be used
   --  during rendering.

   function Subroutine_Indices_Uniform
     (Object : Program;
      Shader : Shaders.Shader_Type;
      Index  : Subroutine_Index_Type) return Subroutine_Index_Array;
   --  Return the indices of compatible subroutines for the given subroutine uniform

   function Subroutine_Uniform_Locations
     (Object : Program;
      Shader : Shaders.Shader_Type) return Size;
   --  Return number of active subroutine uniform locations
   --
   --  All locations between 0 .. Subroutine_Uniform_Locations'Result - 1
   --  are active locations. A subroutine uniform that is an array has one
   --  index, but multiple locations.
   --
   --  This function is used to determine length of array given to
   --  Set_Uniform_Subroutines.

   procedure Set_Uniform_Subroutines (Shader : Shaders.Shader_Type; Indices : UInt_Array)
     with Pre => Indices'First = 0;
   --  Use the given indices of the subroutine functions to set the active
   --  subroutine uniforms
   --
   --  Size of Indices must be equal to Subroutine_Uniform_Locations.
   --  You must call this program after Programs.Use_Program, Pipelines.Bind,
   --  or Pipelines.Use_Program_Stages.
   --
   --  This procedure can be used as follows:
   --
   --  1. Use Subroutine_Uniform_Locations to create a new UInt_Array
   --     with the correct length.
   --  2. Call Subroutine_Index to get the index of a subroutine function.
   --     This will be a *value* in the array.
   --  3. Call Subroutine_Uniform_Location to get the location of a subroutine
   --     uniform. This will be a *key* in the array.
   --  4. Assign the value (function index) to the key (uniform location) in
   --     the array.
   --  5. Repeat steps 2 to 4 for all active subroutine uniforms.

private

   Invalid_Index : constant Subroutine_Index_Type := 16#FFFFFFFF#;

   type Program is new GL_Object with null record;

end GL.Objects.Programs;
