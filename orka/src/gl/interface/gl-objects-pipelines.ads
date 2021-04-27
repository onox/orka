--  SPDX-License-Identifier: Apache-2.0
--
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

private with GL.Low_Level;

package GL.Objects.Pipelines is
   pragma Preelaborate;

   type Stage_Bits is record
      Vertex_Shader          : Boolean := False;
      Fragment_Shader        : Boolean := False;
      Geometry_Shader        : Boolean := False;
      Tess_Control_Shader    : Boolean := False;
      Tess_Evaluation_Shader : Boolean := False;
      Compute_Shader         : Boolean := False;
   end record;

   type Pipeline is new GL_Object with private;

   procedure Use_Program_Stages
     (Object  : Pipeline;
      Stages  : Stage_Bits;
      Program : Programs.Program)
   with Pre => Program.Separable;
   --  Use the specified stages from the given program in the pipeline

   procedure Bind (Object : Pipeline);
   --  Bind the pipeline to the current context

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

   for Stage_Bits use record
      Vertex_Shader          at 0 range 0 .. 0;
      Fragment_Shader        at 0 range 1 .. 1;
      Geometry_Shader        at 0 range 2 .. 2;
      Tess_Control_Shader    at 0 range 3 .. 3;
      Tess_Evaluation_Shader at 0 range 4 .. 4;
      Compute_Shader         at 0 range 5 .. 5;
   end record;
   for Stage_Bits'Size use Low_Level.Bitfield'Size;

end GL.Objects.Pipelines;
