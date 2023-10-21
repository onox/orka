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

   procedure Set_Separable (Subject : Program; Separable : Boolean);
   function Separable (Subject : Program) return Boolean;

   function Compute_Work_Group_Size (Object : Program) return Compute.Dimension_Size_Array;
   --  Size (per dimension) of a local work group in the linked compute stage

   function Unused_Uniform_Location (Subject : Program)
     return Programs.Uniforms.Uniform;

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

private

   type Program is new GL_Object with null record;

end GL.Objects.Programs;
