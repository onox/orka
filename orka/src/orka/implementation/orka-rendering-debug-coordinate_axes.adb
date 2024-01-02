--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2019 onox <denkpadje@gmail.com>
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

with GL.Buffers;
with GL.Types;

with Orka.Rendering.Drawing;
with Orka.Rendering.Programs.Modules;

package body Orka.Rendering.Debug.Coordinate_Axes is

   function Create_Coordinate_Axes
     (Location : Resources.Locations.Location_Ptr) return Coordinate_Axes
   is
      use Rendering.Programs;
   begin
      return Result : Coordinate_Axes :=
        (Program => Create_Program (Modules.Create_Module
           (Location,
            VS => "debug/axes.vert",
            GS => "debug/axes.geom",
            FS => "debug/axes.frag")),
         others  => <>)
      do
         Result.Uniform_View := Result.Program.Uniform ("view");
         Result.Uniform_Proj := Result.Program.Uniform ("proj");
         Result.Uniform_Size := Result.Program.Uniform ("size");
         Result.Uniform_Axis := Result.Program.Uniform ("axisLength");
      end return;
   end Create_Coordinate_Axes;

   procedure Render
     (Object        : in out Coordinate_Axes;
      Width, Height : Positive;
      Axis_Size     : Transforms.Vector4 := (4.0, 100.0, 16.0, 32.0);
      View, Proj    : Transforms.Matrix4;
      Transforms    : Rendering.Buffers.Bindable_Buffer'Class)
   is
      use all type GL.Types.Compare_Function;
      use all type Rendering.Buffers.Indexed_Buffer_Target;

      Original_Function : constant GL.Types.Compare_Function := GL.Buffers.Depth_Function;
   begin
      Object.Uniform_View.Set_Matrix (View);
      Object.Uniform_Proj.Set_Matrix (Proj);
      Object.Uniform_Size.Set_Vector (Unsigned_32_Array'(Unsigned_32 (Width), Unsigned_32 (Height)));
      Object.Uniform_Axis.Set_Vector (Axis_Size);

      Object.Program.Use_Program;

      Transforms.Bind (Shader_Storage, 0);

      GL.Buffers.Set_Depth_Function (Always);
      Orka.Rendering.Drawing.Draw (GL.Types.Lines, 0, 6, Instances => Transforms.Length);
      GL.Buffers.Set_Depth_Function (Original_Function);
   end Render;

end Orka.Rendering.Debug.Coordinate_Axes;
