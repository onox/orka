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

with GL.Objects.Programs;
with GL.Objects.Shaders;

package GL.Objects.Pipelines is
   pragma Preelaborate;

   type Pipeline is new GL_Object with private;

   procedure Use_Program_Stages (Object : Pipeline; Shader : Shaders.Shader_Type;
                                 Program : Programs.Program);
   --  Use the specified shader from the given program in the pipeline
   --
   --  If you have subroutines in some of its shaders, you must
   --  subsequently call Set_Uniform_Subroutines, because the subroutine
   --  state is completely lost after having called Use_Program_Stages.

   procedure Set_Active_Program (Object : Pipeline; Program : Programs.Program);

   procedure Bind (Object : Pipeline);
   --  Bind the pipeline to the current context
   --
   --  If you have subroutines in some of its shaders, you must
   --  subsequently call Set_Uniform_Subroutines, because the subroutine
   --  state is completely lost after having called Bind.

   function Validate (Object : Pipeline) return Boolean;

   function Info_Log (Object : Pipeline) return String;

   overriding
   procedure Initialize_Id (Object : in out Pipeline);

   overriding
   procedure Delete_Id (Object : in out Pipeline);

   overriding
   function Identifier (Object : Pipeline) return Types.Debug.Identifier is
     (Types.Debug.Program_Pipeline);

private

   type Pipeline is new GL_Object with null record;

end GL.Objects.Pipelines;
