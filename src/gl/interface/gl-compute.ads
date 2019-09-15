--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

with GL.Types.Compute;

package GL.Compute is
   pragma Preelaborate;

   use GL.Types;
   use GL.Types.Compute;

   procedure Dispatch_Compute (X, Y, Z : UInt := 1)
     with Pre => X >= 1 and Y >= 1 and Z >= 1;
   --  Launch a compute shader with the given number of work groups
   --  in the X, Y, and Z dimensions
   --
   --  These numbers are available in the shader via the built-in
   --  variable gl_NumWorkGroups.
   --
   --  The number of groups in each dimension in the command must not
   --  be larger than Max_Compute_Work_Group_Count.

   procedure Dispatch_Compute_Indirect (Offset : Size);
   --  Launch a compute shader using the Dispatch_Indirect_Command at the
   --  given offset in the bound Dispatch_Indirect_Buffer
   --
   --  The number of groups in each dimension in the command must not
   --  be larger than Max_Compute_Work_Group_Count.
   --
   --  See GL.Types.Indirect for the type Dispatch_Indirect_Command.

   function Max_Compute_Shared_Memory_Size return Size
     with Post => Max_Compute_Shared_Memory_Size'Result >= 32_768;
   --  Maximum total storage size in bytes of 'shared' variables

   function Max_Compute_Work_Group_Invocations return Size
     with Post => Max_Compute_Work_Group_Invocations'Result >= 1_024;
   --  Maximum total invocations in a single local work group

   function Max_Compute_Work_Group_Count return Dimension_Size_Array
     with Post => (for all Size of Max_Compute_Work_Group_Count'Result => Size >= 65_535);
   --  Maximum number of work groups (per dimension)

   function Max_Compute_Work_Group_Size return Dimension_Size_Array
     with Post => (for all Dimension in Dimension_Size_Array'Range =>
       (case Dimension is
          when X | Y => Max_Compute_Work_Group_Size'Result (Dimension) >= 1_024,
          when Z     => Max_Compute_Work_Group_Size'Result (Dimension) >= 64));
   --  Maximum size (per dimension) of a local work group

end GL.Compute;
