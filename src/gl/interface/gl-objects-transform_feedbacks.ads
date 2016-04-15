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

private with GL.Low_Level.Enums;

package GL.Objects.Transform_Feedbacks is
   pragma Preelaborate;

   type Outputs_Format is (Interleaved_Attributes, Separate_Attributes);

   type Feedback_Target (<>) is tagged limited private;

   Active_Transform_Feedback : constant Feedback_Target;

   type Feedback_Object is new GL_Object with private;

   procedure Bind (Target : Feedback_Target; Object : Feedback_Object'Class);

   function Current (Target : Feedback_Target) return Feedback_Object'Class;

   procedure Draw_Transform_Feedback (Object : Feedback_Object; Mode : Connection_Mode);

   procedure Draw_Transform_Feedback (Object : Feedback_Object;
                                      Mode : Connection_Mode; Instances : Size);

   procedure Draw_Transform_Feedback_Stream (Object : Feedback_Object; Mode : Connection_Mode;
                                             Stream : Natural);

   procedure Draw_Transform_Feedback_Stream (Object : Feedback_Object; Mode : Connection_Mode;
                                             Stream : Natural; Instances : Size);

   overriding
   procedure Initialize_Id (Object : in out Feedback_Object);

   overriding
   procedure Delete_Id (Object : in out Feedback_Object);

   No_Feedback_Object : constant Feedback_Object;

   type Active_Feedback (<>) is limited new Ada.Finalization.Limited_Controlled with private;

   type Paused_Feedback is limited new Ada.Finalization.Limited_Controlled with private;

   function Pause_Feedback (Object : Active_Feedback) return Paused_Feedback'Class;
   --  Pause the current active transform feedback. The value returned
   --  is of a controlled type, meaning you must assign it to some local
   --  variable, so that the transform feedback will be automatically
   --  resumed when the variable goes out of scope.

   subtype Feedback_Connection_Mode is Connection_Mode
     with Static_Predicate => Feedback_Connection_Mode in Points | Lines | Triangles;

   function Begin_Feedback (Object : Feedback_Object;
                            Mode   : Feedback_Connection_Mode) return Active_Feedback'Class;
   --  Start a new transform feedback. The value returned is of a
   --  controlled type, meaning you must assign it to some local
   --  variable, so that the transform feedback will be automatically
   --  ended when the variable goes out of scope.
   --
   --  Depending on Mode a certain number of render primitive modes are
   --  allowed:
   --
   --  Mode      | Render primitive mode
   --  ==========+===============================================
   --  Points    | Points
   --  ----------+-----------------------------------------------
   --  Lines     | Lines, Line_Loop, Line_Strip, Lines_Adjacency,
   --            | Line_Strip_Adjacency
   --  ----------+-----------------------------------------------
   --  Triangles | Triangles, Triangle_Strip, Triangle_Fan,
   --            | Triangles_Adjacency, Triangle_Strip_Adjacency
   --
   --  If a Geometry Shader is active, the transform feedback will
   --  instead capture the output streams in the shader. Depending
   --  on Mode a specific output primitive type is required:
   --
   --  Mode      | Output primitive type
   --  ==========+======================
   --  Points    | points
   --  ----------+----------------------
   --  Lines     | line_strip
   --  ----------+----------------------
   --  Triangles | triangle_strip

private

   for Outputs_Format use (Interleaved_Attributes => 16#8C8C#,
                           Separate_Attributes    => 16#8C8D#);
   for Outputs_Format'Size use Low_Level.Enum'Size;

   type Feedback_Object is new GL_Object with null record;

   type Feedback_Target (Kind : Low_Level.Enums.Transform_Feedback_Kind) is
     tagged limited null record;

   Active_Transform_Feedback : constant Feedback_Target
     := Feedback_Target'(Kind => Low_Level.Enums.Transform_Feedback);

   No_Feedback_Object : constant Feedback_Object :=
     Feedback_Object'(GL_Object with null record);

   type Active_Feedback (Mode : Feedback_Connection_Mode) is limited new Ada.Finalization.Limited_Controlled with record
     Feedback  : Feedback_Object;
     Finalized : Boolean := True;
   end record;

   overriding
   procedure Finalize (Object : in out Active_Feedback);

   type Paused_Feedback is limited new Ada.Finalization.Limited_Controlled with record
     Feedback  : Feedback_Object;
     Finalized : Boolean := True;
   end record;

   overriding
   procedure Finalize (Object : in out Paused_Feedback);

end GL.Objects.Transform_Feedbacks;
