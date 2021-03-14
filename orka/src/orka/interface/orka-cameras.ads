--  SPDX-License-Identifier: Apache-2.0
--
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

with Ada.Numerics;

with GL.Types;

with Orka.Behaviors;
with Orka.Contexts;
with Orka.Inputs.Pointers;
with Orka.Rendering.Framebuffers;
with Orka.Transforms.Singles.Matrices;
with Orka.Transforms.Doubles.Vectors;

private with Orka.Transforms.Doubles.Matrices;
private with Orka.Types;

package Orka.Cameras is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   subtype Vector4 is Orka.Transforms.Doubles.Vectors.Vector4;

   use type GL.Types.Double;

   subtype Angle is GL.Types.Double range -Ada.Numerics.Pi .. Ada.Numerics.Pi;

   subtype Distance is GL.Types.Double range 0.0 .. GL.Types.Double'Last;

   Default_Scale : Vector4 := (0.002, 0.002, 1.0, 0.0);

   -----------------------------------------------------------------------------

   type Camera_Lens (Width, Height : Positive) is tagged private;

   type Lens_Ptr is not null access Camera_Lens'Class;

   procedure Set_FOV (Object : in out Camera_Lens; FOV : GL.Types.Single);

   function FOV (Object : Camera_Lens) return GL.Types.Single;

   function Projection_Matrix (Object : Camera_Lens) return Transforms.Matrix4;

   function Create_Lens
     (Width, Height : Positive;
      FOV : GL.Types.Single;
      Context : Contexts.Context'Class) return Camera_Lens'Class;

   -----------------------------------------------------------------------------

   type Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : not null access constant Rendering.Framebuffers.Framebuffer)
   is abstract tagged private;

   type Camera_Ptr is not null access Camera'Class;

   procedure Update (Object : in out Camera; Delta_Time : Duration) is abstract;

   procedure Set_Input_Scale
     (Object  : in out Camera;
      X, Y, Z : GL.Types.Double);

   procedure Set_Up_Direction
     (Object    : in out Camera;
      Direction : Vector4);

   function View_Matrix (Object : Camera) return Transforms.Matrix4 is abstract;

   function View_Matrix_Inverse (Object : Camera) return Transforms.Matrix4 is abstract;
   --  Return the inverse of the view matrix

   function View_Position (Object : Camera) return Vector4 is abstract;
   --  Return the position of the camera in world space

   function Projection_Matrix (Object : Camera) return Transforms.Matrix4 is
     (Object.Lens.Projection_Matrix)
   with Inline;

   function Create_Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : aliased Rendering.Framebuffers.Framebuffer) return Camera is abstract;

   type Observing_Camera is interface;

   procedure Look_At
     (Object : in out Observing_Camera;
      Target : Behaviors.Behavior_Ptr) is abstract;
   --  Orient the camera such that it looks at the given target

   function Target_Position
     (Object : Observing_Camera) return Vector4 is abstract;
   --  Return the position of the target the camera looks at

   -----------------------------------------------------------------------------
   --                          First person camera's                          --
   -----------------------------------------------------------------------------

   type First_Person_Camera is abstract new Camera with private;

   procedure Set_Position
     (Object   : in out First_Person_Camera;
      Position : Vector4);

   overriding
   function View_Position (Object : First_Person_Camera) return Vector4;

   -----------------------------------------------------------------------------
   --                          Third person camera's                          --
   -----------------------------------------------------------------------------

   type Third_Person_Camera is abstract new Camera and Observing_Camera with private;

   overriding
   procedure Look_At
     (Object : in out Third_Person_Camera;
      Target : Behaviors.Behavior_Ptr);

   overriding
   function Target_Position
     (Object : Third_Person_Camera) return Vector4;

private

   type Camera_Lens (Width, Height : Positive) is tagged record
      FOV : GL.Types.Single;
      Reversed_Z : Boolean;
   end record;

   type Camera
     (Input : Inputs.Pointers.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : not null access constant Rendering.Framebuffers.Framebuffer) is abstract tagged
   record
      Scale : Vector4 := Default_Scale;
      Up    : Vector4 := (0.0, 1.0, 0.0, 0.0);
   end record;

   type First_Person_Camera is abstract new Camera with record
      Position : Vector4 := (0.0, 0.0, 0.0, 1.0);
   end record;

   type Third_Person_Camera is abstract new Camera and Observing_Camera with record
      Target : Behaviors.Behavior_Ptr := Behaviors.Null_Behavior;
   end record;

   function Clamp_Distance  is new Orka.Types.Clamp (GL.Types.Double, Distance);
   function Normalize_Angle is new Orka.Types.Normalize_Periodic (GL.Types.Double, Angle);

   subtype Matrix4 is Orka.Transforms.Doubles.Matrices.Matrix4;

   Y_Axis : constant Vector4 := (0.0, 1.0, 0.0, 0.0);

   function Look_At (Target, Camera, Up_World : Vector4) return Matrix4;

   function Rotate_To_Up (Object : Camera'Class) return Matrix4;

end Orka.Cameras;
