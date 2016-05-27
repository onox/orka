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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Conversion;

with GL.API;

package body GL.Objects.Transform_Feedbacks is

   Error_Checking_Suspended : Boolean := False;

   function Hash (Key : Low_Level.Enums.Transform_Feedback_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Transform_Feedback_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Feedback_Object_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Transform_Feedback_Kind,
       Element_Type => Feedback_Object'Class,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Feedback_Object_Maps.Cursor;

   Current_Feedback_Objects : Feedback_Object_Maps.Map;

   procedure Bind (Target : Feedback_Target; Object : Feedback_Object'Class) is
      Cursor : constant Feedback_Object_Maps.Cursor
        := Current_Feedback_Objects.Find (Target.Kind);
   begin
      if Cursor = Feedback_Object_Maps.No_Element or else
        Feedback_Object_Maps.Element (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
      then
         API.Bind_Transform_Feedback (Target.Kind, Object.Reference.GL_Id);
         if not Error_Checking_Suspended then
            Raise_Exception_On_OpenGL_Error;
         end if;
         if Cursor = Feedback_Object_Maps.No_Element then
            Current_Feedback_Objects.Insert (Target.Kind, Object);
         else
            Current_Feedback_Objects.Replace_Element (Cursor, Object);
         end if;
      end if;
   end Bind;

   procedure Bind_Base (Object : Feedback_Object;
                        Buffer : Objects.Buffers.Buffer'Class;
                        Index  : Natural) is
   begin
      API.Transform_Feedback_Buffer_Base (Object.Reference.GL_Id, UInt (Index), Buffer.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Bind_Base;

   function Current (Target : Feedback_Target) return Feedback_Object'Class is
      Cursor : constant Feedback_Object_Maps.Cursor
        := Current_Feedback_Objects.Find (Target.Kind);
   begin
      if Cursor = Feedback_Object_Maps.No_Element then
         raise No_Object_Bound_Exception with GL.Low_Level.Enums.Transform_Feedback_Kind'Image (Target.Kind);
      else
         return Feedback_Object_Maps.Element (Cursor);
      end if;
   end Current;

   procedure Draw_Transform_Feedback (Object : Feedback_Object; Mode : Connection_Mode) is
   begin
      API.Draw_Transform_Feedback (Mode, Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Transform_Feedback;

   procedure Draw_Transform_Feedback (Object : Feedback_Object;
                                      Mode : Connection_Mode; Instances : Size) is
   begin
      API.Draw_Transform_Feedback_Instanced (Mode, Object.Reference.GL_Id,
                                             Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Transform_Feedback;

   procedure Draw_Transform_Feedback_Stream (Object : Feedback_Object; Mode : Connection_Mode;
                                             Stream : Natural) is
   begin
      API.Draw_Transform_Feedback_Stream (Mode, Object.Reference.GL_Id, UInt (Stream));
      Raise_Exception_On_OpenGL_Error;
   end Draw_Transform_Feedback_Stream;

   procedure Draw_Transform_Feedback_Stream (Object : Feedback_Object; Mode : Connection_Mode;
                                             Stream : Natural; Instances : Size) is
   begin
      API.Draw_Transform_Feedback_Stream_Instanced (Mode, Object.Reference.GL_Id,
                                                    UInt (Stream), Instances);
      Raise_Exception_On_OpenGL_Error;
   end Draw_Transform_Feedback_Stream;

   overriding
   procedure Initialize_Id (Object : in out Feedback_Object) is
      New_Id : UInt := 0;
   begin
      API.Create_Transform_Feedbacks (1, New_Id);
      Raise_Exception_On_OpenGL_Error;

      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Feedback_Object) is
   begin
      API.Delete_Transform_Feedbacks (1, (1 => Object.Reference.GL_Id));
      Raise_Exception_On_OpenGL_Error;

      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   function Begin_Feedback (Object : Feedback_Object; Mode : Feedback_Connection_Mode)
     return Active_Feedback'Class is
   begin
      API.Begin_Transform_Feedback (Mode);
      Raise_Exception_On_OpenGL_Error;
      Error_Checking_Suspended := True;
      return Active_Feedback'(Ada.Finalization.Limited_Controlled
        with Feedback => Object, Mode => Mode, Finalized => False);
   end Begin_Feedback;

   overriding
   procedure Finalize (Object : in out Active_Feedback) is
   begin
      if not Object.Finalized then
         API.End_Transform_Feedback;
         Error_Checking_Suspended := False;
         Object.Finalized := True;
      end if;
   end Finalize;

   function Pause_Feedback (Object : Active_Feedback) return Paused_Feedback'Class is
   begin
      API.Pause_Transform_Feedback;
      Raise_Exception_On_OpenGL_Error;
      Error_Checking_Suspended := True;
      return Paused_Feedback'(Ada.Finalization.Limited_Controlled
        with Feedback => Object.Feedback, Finalized => False);
   end Pause_Feedback;

   overriding
   procedure Finalize (Object : in out Paused_Feedback) is
   begin
      if not Object.Finalized then
         --  Re-bind if no longer bound
         if Active_Transform_Feedback.Current /= Feedback_Object'Class (Object.Feedback) then
            Active_Transform_Feedback.Bind (Object.Feedback);
         end if;
         API.Resume_Transform_Feedback;
         Error_Checking_Suspended := False;
         Object.Finalized := True;
      end if;
   end Finalize;

end GL.Objects.Transform_Feedbacks;
