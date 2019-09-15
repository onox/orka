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

with GL.Fences;

generic
   type Index_Type is mod <>;
   Maximum_Wait : Duration := 0.010;
package Orka.Rendering.Fences is

   type Buffer_Fence is tagged private;

   type Fence_Status is (Not_Initialized, Signaled, Not_Signaled);

   function Create_Buffer_Fence return Buffer_Fence;

   procedure Prepare_Index (Object : in out Buffer_Fence; Status : out Fence_Status);
   --  Perform a client wait sync for the fence corresponding to the
   --  current index

   procedure Advance_Index (Object : in out Buffer_Fence);
   --  Set a fence for the corresponding index and then increment it

private

   type Fence_Array is array (Index_Type) of GL.Fences.Fence;

   type Buffer_Fence is tagged record
      Fences : Fence_Array;
      Index  : Index_Type;
   end record;

end Orka.Rendering.Fences;
