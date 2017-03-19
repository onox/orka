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

with Ada.Containers.Hashed_Sets;
with Ada.Real_Time;

with Orka.Behaviors;
with Orka.Transforms.SIMD_Vectors;
with Orka.Windows;

generic
   Time_Step, Initial_Frame_Limit : Ada.Real_Time.Time_Span;
   Window : access Orka.Windows.Window'Class;
   with package Transforms is new Orka.Transforms.SIMD_Vectors (<>);
package Orka.Loops is

   protected Handler is
      procedure Stop;

      procedure Set_Frame_Limit (Value : Ada.Real_Time.Time_Span);

      function Frame_Limit return Ada.Real_Time.Time_Span;

      function Should_Stop return Boolean;
   private
      Limit : Ada.Real_Time.Time_Span := Initial_Frame_Limit;
      Stop_Flag : Boolean := False;
   end Handler;

   use type Behaviors.Behavior_Ptr;

   function Behavior_Hash (Element : Behaviors.Behavior_Ptr) return Ada.Containers.Hash_Type;

   package Behavior_Sets is new Ada.Containers.Hashed_Sets (Behaviors.Behavior_Ptr, Behavior_Hash, "=");

   type Behavior_Array is array (Positive range <>) of Behaviors.Behavior_Ptr;

   type Behavior_Array_Access is access Behavior_Array;

   protected Scene is
      procedure Add (Object : Behaviors.Behavior_Ptr);

      procedure Remove (Object : Behaviors.Behavior_Ptr);

      procedure Replace_Array (Target : in out Behavior_Array_Access)
        with Pre => Modified;

      function Modified return Boolean;
   private
      Modified_Flag : Boolean := False;
      Behaviors_Set : Behavior_Sets.Set;
   end Scene;

   task Game_Loop;

end Orka.Loops;
