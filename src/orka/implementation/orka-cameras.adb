--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

package body Orka.Cameras is

   procedure Set_FOV (Object : in out Camera_Lens; FOV : GL.Types.Single) is
   begin
      Object.FOV := FOV;
   end Set_FOV;

   function FOV (Object : Camera_Lens) return GL.Types.Single is
     (Object.FOV);

   function Projection_Matrix (Object : Camera_Lens) return Transforms.Matrix4 is
      Width  : constant GL.Types.Single := GL.Types.Single (Object.Width);
      Height : constant GL.Types.Single := GL.Types.Single (Object.Height);
   begin
      return Transforms.Infinite_Perspective_Reversed_Z (Object.FOV, Width / Height, 0.1);
   end Projection_Matrix;

   -----------------------------------------------------------------------------

   procedure Set_Position
     (Object   : in out First_Person_Camera;
      Position : Transforms.Vector4) is
   begin
      Object.Position := Position;
   end Set_Position;

   overriding
   procedure Look_At
     (Object : in out Third_Person_Camera;
      Target : Behaviors.Behavior_Ptr) is
   begin
      Object.Target := Target;
   end Look_At;

   -----------------------------------------------------------------------------

   overriding
   function View_Position (Object : First_Person_Camera) return Transforms.Vector4 is
     (Object.Position);

   overriding
   function View_Position (Object : Third_Person_Camera) return Transforms.Vector4 is
     (Object.Target.Position);

   -----------------------------------------------------------------------------

   function Create_Lens
     (Width, Height : Positive;
      FOV : GL.Types.Single;
      Context : Contexts.Context) return Camera_Lens'Class is
   begin
      return Result : Camera_Lens (Width, Height) do
         Result.FOV := FOV;
      end return;
   end Create_Lens;

end Orka.Cameras;
