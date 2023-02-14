--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with GL.Low_Level;

package GL.Objects.Queries is
   pragma Preelaborate;

   type Query_Type is
     (Vertices_Submitted,
      Primitives_Submitted,
      Vertex_Shader_Invocations,
      Tess_Control_Shader_Invocations,
      Tess_Evaluation_Shader_Invocations,
      Geometry_Shader_Primitives_Emitted,
      Fragment_Shader_Invocations,
      Compute_Shader_Invocations,
      Clipping_Input_Primitives,
      Clipping_Output_Primitives,
      Geometry_Shader_Invocations,
      Time_Elapsed,
      Samples_Passed,
      Any_Samples_Passed,
      Primitives_Generated,
      Any_Samples_Passed_Conservative,
      Timestamp);

   --  Has to be defined here because of the subtype declaration below
   for Query_Type use
     (Vertices_Submitted                    => 16#82EE#,
      Primitives_Submitted                  => 16#82EF#,
      Vertex_Shader_Invocations             => 16#82F0#,
      Tess_Control_Shader_Invocations       => 16#82F1#,
      Tess_Evaluation_Shader_Invocations    => 16#82F2#,
      Geometry_Shader_Primitives_Emitted    => 16#82F3#,
      Fragment_Shader_Invocations           => 16#82F4#,
      Compute_Shader_Invocations            => 16#82F5#,
      Clipping_Input_Primitives             => 16#82F6#,
      Clipping_Output_Primitives            => 16#82F7#,
      Geometry_Shader_Invocations           => 16#887F#,
      Time_Elapsed                          => 16#88BF#,
      Samples_Passed                        => 16#8914#,
      Any_Samples_Passed                    => 16#8C2F#,
      Primitives_Generated                  => 16#8C87#,
      Any_Samples_Passed_Conservative       => 16#8D6A#,
      Timestamp                             => 16#8E28#);
   for Query_Type'Size use Low_Level.Enum'Size;

   subtype Async_Query_Type is Query_Type
     range Vertices_Submitted .. Any_Samples_Passed_Conservative;

   subtype Timestamp_Query_Type is Query_Type range Timestamp .. Timestamp;

   subtype Stream_Query_Type is Query_Type
     with Static_Predicate =>
       Stream_Query_Type in Primitives_Generated;

   type Query_Param is (Result, Result_Available, Result_No_Wait);

   type Query (Target : Query_Type) is new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Query);

   overriding
   procedure Delete_Id (Object : in out Query);

   overriding
   function Identifier (Object : Query) return Types.Debug.Identifier is
     (Types.Debug.Query);

   type Active_Query is limited new Ada.Finalization.Limited_Controlled with private;

   function Begin_Query
     (Object : in out Query;
      Index  : in     Natural := 0) return Active_Query'Class
   with Pre => Object.Target in Async_Query_Type and
     (if Object.Target not in Stream_Query_Type then Index = 0);
   --  Start an asynchronous query. The value returned is of a controlled
   --  type, meaning you must assign it to some local variable, so that
   --  the query will be automatically ended when the variable goes out
   --  of scope.
   --
   --  Queries of type Timestamp are not used within a scope. For such
   --  a query you can record the time into the query object by calling
   --  Record_Current_Time.
   --
   --  Certain queries support multiple query operations; one for each
   --  index. The index represents the vertex output stream used in a
   --  Geometry Shader. These targets are:
   --
   --    * Primitives_Generated

   function Result_Available (Object : in out Query) return Boolean;
   --  Return true if a result is available, false otherwise. This function
   --  can be used to avoid calling Result (and thereby stalling the CPU)
   --  when the result is not yet available.

   function Result_If_Available (Object : in out Query; Default : Boolean) return Boolean;
   function Result_If_Available (Object : in out Query; Default : Natural) return Natural;
   function Result_If_Available (Object : in out Query; Default : UInt64)  return UInt64;
   --  Return the result if available, otherwise return the default value

   function Result (Object : in out Query) return Boolean;
   function Result (Object : in out Query) return Natural;
   function Result (Object : in out Query) return UInt64;
   --  Return the result. If the result is not yet available, then the
   --  CPU will stall until the result becomes available. This means
   --  that if you do not call Result_Available, then this function call
   --  will make the query synchronous.

   procedure Record_Current_Time (Object : in out Query);
   --  Record the time when the GPU has completed all previous commands
   --  in a query object. The result must be retrieved asynchronously using
   --  one of the Result functions.

   function Get_Current_Time return Long;
   --  Return the time when the GPU has received (but not necessarily
   --  completed) all previous commands. Calling this function stalls the CPU.

private

   for Query_Param use (Result           => 16#8866#,
                        Result_Available => 16#8867#,
                        Result_No_Wait   => 16#9194#);
   for Query_Param'Size use Low_Level.Enum'Size;

   type Query (Target : Query_Type) is new GL_Object with null record;

   type Active_Query is limited new Ada.Finalization.Limited_Controlled with record
      Target : Query_Type;
      Index  : Natural;
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Active_Query);

end GL.Objects.Queries;
