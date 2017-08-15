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
     (Transform_Feedback_Overflow,
      Transform_Feedback_Stream_Overflow,
      Time_Elapsed,
      Samples_Passed,
      Any_Samples_Passed,
      Primitives_Generated,
      Transform_Feedback_Primitives_Written,
      Any_Samples_Passed_Conservative,
      Timestamp);

   --  Has to be defined here because of the subtype declaration below
   for Query_Type use
     (Transform_Feedback_Overflow           => 16#82EC#,
      Transform_Feedback_Stream_Overflow    => 16#82ED#,
      Time_Elapsed                          => 16#88BF#,
      Samples_Passed                        => 16#8914#,
      Any_Samples_Passed                    => 16#8C2F#,
      Primitives_Generated                  => 16#8C87#,
      Transform_Feedback_Primitives_Written => 16#8C88#,
      Any_Samples_Passed_Conservative       => 16#8D6A#,
      Timestamp                             => 16#8E28#);
   for Query_Type'Size use Low_Level.Enum'Size;

   subtype Async_Query_Type is Query_Type
     range Transform_Feedback_Overflow .. Any_Samples_Passed_Conservative;

   subtype Timestamp_Query_Type is Query_Type range Timestamp .. Timestamp;

   subtype Stream_Query_Type is Query_Type
     with Static_Predicate =>
       Stream_Query_Type in Primitives_Generated |
                            Transform_Feedback_Primitives_Written |
                            Transform_Feedback_Stream_Overflow;

   type Query_Mode is (Wait, No_Wait, By_Region_Wait, By_Region_No_Wait,
                       Wait_Inverted, No_Wait_Inverted, By_Region_Wait_Inverted,
                       By_Region_No_Wait_Inverted);

   type Query_Param is (Result, Result_Available, Result_No_Wait);

   type Target_Param is (Counter_Bits, Current_Query);

   type Query (Target : Query_Type) is new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Query);

   overriding
   procedure Delete_Id (Object : in out Query);

   overriding
   function Identifier (Object : Query) return Types.Debug.Identifier is
     (Types.Debug.Query);

   type Active_Query is limited new Ada.Finalization.Limited_Controlled with private;

   type Conditional_Render is limited new Ada.Finalization.Limited_Controlled with private;

   function Begin_Query
     (Object : in out Query;
      Target : in     Async_Query_Type;
      Index  : in     Natural := 0) return Active_Query'Class
   with Pre => (if Target not in Stream_Query_Type then Index = 0);
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
   --    * Transform_Feedback_Primitives_Written
   --    * Transform_Feedback_Stream_Overflow
   --
   --  The query should be issued while within a transform feedback scope
   --  for one of the following targets:
   --
   --    * Transform_Feedback_Primitives_Written
   --    * Transform_Feedback_Overflow
   --    * Transform_Feedback_Stream_Overflow

   function Begin_Conditional_Render (Object : in out Query;
                                      Mode   : in     Query_Mode)
     return Conditional_Render'Class;
   --  Start a conditional rendering. The value returned is of a controlled
   --  type, meaning you must assign it to some local variable, so that
   --  the rendering will be automatically ended when the variable goes
   --  out of scope.
   --
   --  If Mode is (By_Region_)Wait, then OpenGL will wait until the result
   --  becomes available and uses the value of the result to determine
   --  whether to execute or discard rendering commands.
   --
   --  If Mode is *_Inverted, then the condition is inverted, meaning it
   --  will execute rendering commands if the query result is zero/false.
   --
   --  If Mode is (By_Region_)No_Wait(_Inverted), then OpenGL may choose
   --  to execute rendering commands while the result of the query is not
   --  available.

   function Result_Available (Object : in out Query) return Boolean;
   --  Return true if a result is available, false otherwise. This function
   --  can be used to avoid calling Result (and thereby stalling the CPU)
   --  when the result is not yet available.

   function Result_If_Available (Object : in out Query; Default_Value : Boolean)
     return Boolean;
   --  Return the result if available, otherwise return the default value

   function Result_If_Available (Object : in out Query; Default_Value : Natural)
     return Natural;
   --  Return the result if available, otherwise return the default value

   function Result (Object : in out Query) return Boolean;
   --  Return the result. If the result is not yet available, then the
   --  CPU will stall until the result becomes available. This means
   --  that if you do not call Result_Available, then this function call
   --  will make the query synchronous.

   function Result (Object : in out Query) return Natural;
   --  Return the result. If the result is not yet available, then the
   --  CPU will stall until the result becomes available. This means
   --  that if you do not call Result_Available, then this function call
   --  will make the query synchronous.

   function Result_Bits (Target : in Query_Type) return Natural;

   procedure Record_Current_Time (Object : in out Query);
   --  Record the time when the GPU has completed all previous commands
   --  in a query object. The result must be retrieved asynchronously using
   --  one of the Result functions.

   function Get_Current_Time return Long;
   --  Return the time when the GPU has received (but not necessarily
   --  completed) all previous commands. Calling this function stalls the CPU.

private

   for Query_Mode use (Wait                       => 16#8E13#,
                       No_Wait                    => 16#8E14#,
                       By_Region_Wait             => 16#8E15#,
                       By_Region_No_Wait          => 16#8E16#,
                       Wait_Inverted              => 16#8E17#,
                       No_Wait_Inverted           => 16#8E18#,
                       By_Region_Wait_Inverted    => 16#8E19#,
                       By_Region_No_Wait_Inverted => 16#8E1A#);
   for Query_Mode'Size use Low_Level.Enum'Size;

   for Target_Param use (Counter_Bits  => 16#8864#,
                         Current_Query => 16#8865#);
   for Target_Param'Size use Low_Level.Enum'Size;

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

   type Conditional_Render is limited new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean;
   end record;

   overriding
   procedure Finalize (Object : in out Conditional_Render);

end GL.Objects.Queries;
