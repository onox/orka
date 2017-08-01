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

with GL.Types;

with Orka.Behaviors;
with Orka.Framebuffers;
with Orka.Inputs;
with Orka.Transforms.Singles.Matrices;

package Orka.Cameras is
   pragma Preelaborate;

   package Transforms renames Orka.Transforms.Singles.Matrices;

   use type GL.Types.Single;

   subtype Angle is GL.Types.Single range -180.0 .. 180.0;

   subtype Distance is GL.Types.Single range 0.0 .. GL.Types.Single'Last;

   -----------------------------------------------------------------------------

   type Camera_Lens (Width, Height : Positive) is tagged private;

   type Lens_Ptr is not null access Camera_Lens'Class;

   procedure Set_FOV (Object : in out Camera_Lens; FOV : GL.Types.Single);

   function FOV (Object : Camera_Lens) return GL.Types.Single;

   function Projection_Matrix (Object : Camera_Lens) return Transforms.Matrix4;

   function Create_Lens
     (Width, Height : Positive;
      FOV : GL.Types.Single) return Camera_Lens'Class;

   -----------------------------------------------------------------------------

   type Camera
     (Input : Inputs.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Framebuffers.Framebuffer_Ptr) is abstract tagged private;

   type Camera_Ptr is not null access Camera'Class;

   procedure Update (Object : in out Camera; Delta_Time : Duration) is abstract;

   function View_Matrix (Object : Camera) return Transforms.Matrix4 is abstract;

   function View_Position (Object : Camera) return Transforms.Vector4 is abstract;
   --  Return the position of the camera in world space

   function Projection_Matrix (Object : Camera) return Transforms.Matrix4 is
     (Object.Lens.Projection_Matrix)
   with Inline;

   type Parameter_Record is record
      Input : Inputs.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Framebuffers.Framebuffer_Ptr;
   end record;

   function Create_Camera
     (Parameters : not null access Parameter_Record) return Camera is abstract;

   type Camera_Kind is (Look_From, Look_At, Rotate_Around, Follow);

   function Create_Camera
     (Kind  : Camera_Kind;
      Input : Inputs.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Framebuffers.Framebuffer_Ptr) return Camera'Class;

   type Observing_Camera is interface;

   procedure Look_At
     (Object : in out Observing_Camera;
      Target : Behaviors.Behavior_Ptr) is abstract;
   --  Orient the camera such that it looks at the given target

   -----------------------------------------------------------------------------
   --                          First person camera's                          --
   -----------------------------------------------------------------------------

   type First_Person_Camera is abstract new Camera with private;

   procedure Set_Position
     (Object   : in out First_Person_Camera;
      Position : Transforms.Vector4);

   overriding
   function View_Position (Object : First_Person_Camera) return Transforms.Vector4;

   type Look_From_Camera is new First_Person_Camera with private;

   procedure Set_Orientation
     (Object : in out Look_From_Camera;
      Roll, Pitch, Yaw : Angle);

   overriding
   function View_Matrix (Object : Look_From_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Look_From_Camera; Delta_Time : Duration);

   type Look_At_Camera is new First_Person_Camera and Observing_Camera with private;

   overriding
   procedure Look_At
     (Object : in out Look_At_Camera;
      Target : Behaviors.Behavior_Ptr);

   procedure Set_Up_Direction
     (Object    : in out Look_At_Camera;
      Direction : Transforms.Vector4);

   overriding
   function View_Matrix (Object : Look_At_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Look_At_Camera; Delta_Time : Duration) is null;
   --  Look_At camera does not need to implement Update because the
   --  view matrix does not depend on the pointer (it is computed using
   --  the camera's and target's positions)

   -----------------------------------------------------------------------------
   --                          Third person camera's                          --
   -----------------------------------------------------------------------------

   type Third_Person_Camera is abstract new Camera and Observing_Camera with private;

   overriding
   procedure Look_At
     (Object : in out Third_Person_Camera;
      Target : Behaviors.Behavior_Ptr);

   overriding
   function View_Position (Object : Third_Person_Camera) return Transforms.Vector4;

   type Rotate_Around_Camera is new Third_Person_Camera with private;

   procedure Set_Angles
     (Object : in out Rotate_Around_Camera;
      Alpha  : Angle;
      Beta   : Angle);

   procedure Set_Radius
     (Object : in out Rotate_Around_Camera;
      Radius : Distance);

   overriding
   function View_Matrix (Object : Rotate_Around_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Rotate_Around_Camera; Delta_Time : Duration);

   type Follow_Camera is new Third_Person_Camera with private;

   procedure Set_Radius (Object : in out Follow_Camera; Radius : Distance);

   procedure Set_Height (Object : in out Follow_Camera; Height : Distance);

   procedure Set_Direction
     (Object    : in out Follow_Camera;
      Direction : Angle);

   overriding
   function View_Matrix (Object : Follow_Camera) return Transforms.Matrix4;

   overriding
   procedure Update (Object : in out Follow_Camera; Delta_Time : Duration);

   -----------------------------------------------------------------------------

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Look_From_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Look_At_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Rotate_Around_Camera;

   overriding
   function Create_Camera
     (Parameters : not null access Parameter_Record) return Follow_Camera;

private

   type Camera_Lens (Width, Height : Positive) is tagged record
      FOV : GL.Types.Single;
   end record;

   type Camera
     (Input : Inputs.Pointer_Input_Ptr;
      Lens  : Lens_Ptr;
      FB    : Framebuffers.Framebuffer_Ptr) is abstract tagged null record;

   type First_Person_Camera is abstract new Camera with record
      Position : Transforms.Vector4 := (0.0, 0.0, 0.0, 1.0);
   end record;

   type Look_From_Camera is new First_Person_Camera with record
      Roll, Pitch, Yaw : Angle := 0.0;
   end record;

   type Look_At_Camera is new First_Person_Camera and Observing_Camera with record
      Target : Behaviors.Behavior_Ptr := Behaviors.Null_Behavior;
      Up     : Transforms.Vector4 := (0.0, 1.0, 0.0, 0.0);
   end record;

   type Third_Person_Camera is abstract new Camera and Observing_Camera with record
      Target : Behaviors.Behavior_Ptr := Behaviors.Null_Behavior;
   end record;

   type Rotate_Around_Camera is new Third_Person_Camera with record
      Alpha  : Angle := 0.0;
      Beta   : Angle := 0.0;
      Radius : Distance := 1.0;
   end record;

   type Follow_Camera is new Third_Person_Camera with record
      Height, Radius : Distance := 1.0;
      Direction      : Angle := 0.0;
   end record;

end Orka.Cameras;
