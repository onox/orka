--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

with Orka.Transforms.Doubles.Quaternions;
with Orka.Transforms.Doubles.Vectors;

private with Orka.Numerics.Doubles.Tensors.CPU;
private with Orka.Numerics.Kalman.CDKF;

private package AWT.IMUs is
   pragma Preelaborate;

   package Quaternions renames Orka.Transforms.Doubles.Quaternions;
   package Vectors     renames Orka.Transforms.Doubles.Vectors;

   type Estimated_State is record
      Orientation      : Quaternions.Quaternion;
      Angular_Velocity : Vectors.Direction;
   end record;
   --  Estimated true angular velocity is (pitch up, yaw left, roll left) in rad/s

   type IMU is tagged private;

   procedure Integrate
     (Object       : in out IMU;
      Velocity     : Vectors.Direction;
      Acceleration : Vectors.Vector4;
      DT           : Duration;
      State        : out Estimated_State;
      Calibrated   : out Boolean);
   --  Perform numerical integration using the given angular velocity and linear acceleration
   --
   --  Angular velocity should be (pitch up, yaw left, roll left, 0) in rad/s
   --  and linear acceleration should be (right, up, forward, 0) in g's where
   --  g is the gravitational acceleration constant (1 g = 9.81 m/s^2).

   procedure Initialize
     (Object      : in out IMU;
      Orientation : Quaternions.Quaternion := Quaternions.Identity);
   --  Initialize the whole filter and reset the orientation to
   --  the given value
   --
   --  To reset the orientation at any time after having initialized
   --  the IMU, call procedure Reset.

   procedure Reset
     (Object      : in out IMU;
      Orientation : Quaternions.Quaternion := Quaternions.Identity);
   --  Reset the orientation to the given value

private

   use Orka.Numerics.Doubles.Tensors.CPU;

   package Kalman is new Orka.Numerics.Kalman (Orka.Numerics.Doubles.Tensors, CPU_Tensor);
   package CDKF   is new Kalman.CDKF;

   type Direction_Array is array (Positive range <>) of Vectors.Direction;

   type IMU is tagged record
      Filter : Kalman.Filter
        (Kind        => Kalman.Filter_CDKF,
         Dimension_X => 7,
         Dimension_Z => 3);

      --  Measurements of angular velocity to calibrate gyro bias
      Velocity_Measurements : Direction_Array (1 .. 1_000);
      Velocity_Index        : Natural;
   end record;

end AWT.IMUs;
