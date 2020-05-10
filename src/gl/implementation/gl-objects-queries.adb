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

with GL.API;
with GL.Enums.Getter;

package body GL.Objects.Queries is

   overriding
   procedure Initialize_Id (Object : in out Query) is
      New_Id : UInt := 0;
   begin
      API.Create_Queries.Ref (Object.Target, 1, New_Id);
      Object.Reference.GL_Id := New_Id;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Query) is
   begin
      API.Delete_Queries.Ref (1, (1 => Object.Reference.GL_Id));
      Object.Reference.GL_Id := 0;
   end Delete_Id;

   function Begin_Query
     (Object : in out Query;
      Index  : in     Natural := 0) return Active_Query'Class is
   begin
      API.Begin_Query_Indexed.Ref (Object.Target, UInt (Index), Object.Reference.GL_Id);
      return Active_Query'(Ada.Finalization.Limited_Controlled
        with Target => Object.Target, Index => Index, Finalized => False);
   end Begin_Query;

   overriding
   procedure Finalize (Object : in out Active_Query) is
   begin
      if not Object.Finalized then
         API.End_Query_Indexed.Ref (Object.Target, UInt (Object.Index));
         Object.Finalized := True;
      end if;
   end Finalize;

   function Begin_Conditional_Render (Object : in out Query;
                                      Mode   : in     Query_Mode)
     return Conditional_Render'Class is
   begin
      API.Begin_Conditional_Render.Ref (Object.Reference.GL_Id, Mode);
      return Conditional_Render'(Ada.Finalization.Limited_Controlled
        with Finalized => False);
   end Begin_Conditional_Render;

   overriding
   procedure Finalize (Object : in out Conditional_Render) is
   begin
      if not Object.Finalized then
         API.End_Conditional_Render.Ref.all;
         Object.Finalized := True;
      end if;
   end Finalize;

   function Result_Available (Object : in out Query) return Boolean is
      Available : UInt := 0;
   begin
      API.Get_Query_Object_UInt.Ref (Object.Reference.GL_Id, Result_Available, Available);
      return Available = 1;
   end Result_Available;

   -----------------------------------------------------------------------------

   function Result_If_Available (Object : in out Query; Default : Boolean) return Boolean is
      Result : UInt := (if Default then 1 else 0);
   begin
      API.Get_Query_Object_UInt.Ref (Object.Reference.GL_Id, Result_No_Wait, Result);
      return Result = 1;
   end Result_If_Available;

   function Result_If_Available (Object : in out Query; Default : Natural) return Natural is
      Result : UInt := UInt (Default);
   begin
      API.Get_Query_Object_UInt.Ref (Object.Reference.GL_Id, Result_No_Wait, Result);
      return Natural (Result);
   end Result_If_Available;

   function Result_If_Available (Object : in out Query; Default : UInt64) return UInt64 is
      Result : UInt64 := Default;
   begin
      API.Get_Query_Object_UInt64.Ref (Object.Reference.GL_Id, Result_No_Wait, Result);
      return Result;
   end Result_If_Available;

   -----------------------------------------------------------------------------

   function Result (Object : in out Query) return Boolean is
      Result_Value : UInt := 0;
   begin
      API.Get_Query_Object_UInt.Ref (Object.Reference.GL_Id, Result, Result_Value);
      return Result_Value = 1;
   end Result;

   function Result (Object : in out Query) return Natural is
      Result_Value : UInt := 0;
   begin
      API.Get_Query_Object_UInt.Ref (Object.Reference.GL_Id, Result, Result_Value);
      return Natural (Result_Value);
   end Result;

   function Result (Object : in out Query) return UInt64 is
      Result_Value : UInt64 := 0;
   begin
      API.Get_Query_Object_UInt64.Ref (Object.Reference.GL_Id, Result, Result_Value);
      return Result_Value;
   end Result;

   -----------------------------------------------------------------------------

   function Result_Bits (Target : in Query_Type) return Natural is
      Bits : Int := 0;
   begin
      API.Get_Query_Indexed_Param.Ref (Target, 0, Counter_Bits, Bits);
      return Natural (Bits);
   end Result_Bits;

   procedure Record_Current_Time (Object : in out Query) is
   begin
      API.Query_Counter.Ref (Object.Reference.GL_Id, Timestamp);
   end Record_Current_Time;

   function Get_Current_Time return Long is
      Result : Types.Long := 0;
   begin
      API.Get_Long.Ref (Enums.Getter.Timestamp, Result);
      return Result;
   end Get_Current_Time;

end GL.Objects.Queries;
