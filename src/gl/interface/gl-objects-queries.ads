--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.Low_Level;

package GL.Objects.Queries is
   pragma Preelaborate;

   type Query_Type is (Time_Elapsed,
                       Samples_Passed, Any_Samples_Passed,
                       Primitives_Generated,
                       Transform_Feedback_Primitives_Written,
                       Any_Samples_Passed_Conservative,
                       Timestamp);

   --  Has to be defined here because of the subtype declaration below
   for Query_Type use (Time_Elapsed                          => 16#88BF#,
                       Samples_Passed                        => 16#8914#,
                       Any_Samples_Passed                    => 16#8C2F#,
                       Primitives_Generated                  => 16#8C87#,
                       Transform_Feedback_Primitives_Written => 16#8C88#,
                       Any_Samples_Passed_Conservative       => 16#8D6A#,
                       Timestamp                             => 16#8E28#);
   for Query_Type'Size use Low_Level.Enum'Size;

   subtype Async_Query_Type is Query_Type range Time_Elapsed .. Any_Samples_Passed_Conservative;

   subtype Timestamp_Query_Type is Query_Type range Timestamp .. Timestamp;

   subtype Primitive_Query_Type is Query_Type
     with Static_Predicate =>
       Primitive_Query_Type in Primitives_Generated | Transform_Feedback_Primitives_Written;

   subtype Occlusion_Query_Type is Query_Type
     with Static_Predicate =>
       Occlusion_Query_Type in Samples_Passed | Any_Samples_Passed | Any_Samples_Passed_Conservative;

   subtype Time_Query_Type is Query_Type
     with Static_Predicate => Time_Query_Type = Time_Elapsed;

   type Query_Mode is (Wait, No_Wait, By_Region_Wait, By_Region_No_Wait,
                       Wait_Inverted, No_Wait_Inverted, By_Region_Wait_Inverted,
                       By_Region_No_Wait_Inverted);

   type Query_Param is (Result, Result_Available, Result_No_Wait);

   type Target_Param is (Counter_Bits, Current_Query);

   type Query is new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Query);

   overriding
   procedure Delete_Id (Object : in out Query);

   type Active_Query is limited new Ada.Finalization.Limited_Controlled with private;

   type Conditional_Render is limited new Ada.Finalization.Limited_Controlled with private;

   function Begin_Primitive_Query (Object : in out Query;
                                   Target : in     Primitive_Query_Type;
                                   Index  : in     Natural := 0)
     return Active_Query'Class;
   --  Start a primitive query. The value returned is of a controlled
   --  type, meaning you must assign it to some local variable, so that
   --  the query will be automatically ended when the variable goes out
   --  of scope.
   --
   --  Primitive queries support multiple query operations; one for each
   --  index. The index represents the vertex output stream used in a
   --  Geometry Shader. If the target type is
   --  Transform_Feedback_Primitives_Written, then the query should be
   --  issued while within a transform feedback scope.

   function Begin_Occlusion_Query (Object : in out Query;
                                   Target : in     Occlusion_Query_Type)
     return Active_Query'Class;
   --  Start an occlusion query. The value returned is of a controlled
   --  type, meaning you must assign it to some local variable, so that
   --  the query will be automatically ended when the variable goes out
   --  of scope.

   function Begin_Timer_Query (Object : in out Query;
                               Target : in     Time_Query_Type)
     return Active_Query'Class;
   --  Start a timer query. The value returned is of a controlled
   --  type, meaning you must assign it to some local variable, so that
   --  the query will be automatically ended when the variable goes out
   --  of scope.
   --
   --  This function is used only for queries of type Time_Elapsed.
   --  Queries of type Timestamp are not used within a scope. For such
   --  a query you can record the time into the query object by calling
   --  Record_Current_Time.

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

   type Query is new GL_Object with null record;

   type Active_Query is limited new Ada.Finalization.Limited_Controlled with record
     Target : Query_Type;
     Index  : Natural;
     Finalized : Boolean := True;
   end record;

   overriding
   procedure Finalize (Object : in out Active_Query);

   type Conditional_Render is limited new Ada.Finalization.Limited_Controlled with record
     Finalized : Boolean := True;
   end record;

   overriding
   procedure Finalize (Object : in out Conditional_Render);

end GL.Objects.Queries;
