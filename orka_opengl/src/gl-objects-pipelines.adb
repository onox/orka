--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2015 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;

with GL.API;
with GL.Enums;

package body GL.Objects.Pipelines is

   procedure Use_Program_Stages
     (Object  : Pipeline;
      Stages  : Stage_Bits;
      Program : Programs.Program)
   is
      use type Low_Level.Bitfield;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Stage_Bits, Target => Low_Level.Bitfield);
      Raw_Bits : constant Low_Level.Bitfield :=
        Convert (Stages) and 2#0000000000111111#;
   begin
      API.Use_Program_Stages.Ref (Object.Reference.GL_Id, Raw_Bits, Program.Raw_Id);
   end Use_Program_Stages;

   procedure Bind (Object : Pipeline) is
   begin
      API.Bind_Program_Pipeline.Ref (Object.Reference.GL_Id);
   end Bind;

   function Validate (Object : Pipeline) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Validate_Program_Pipeline.Ref (Object.Reference.GL_Id);
      API.Get_Program_Pipeline_Param.Ref
        (Object.Reference.GL_Id, Enums.Validate_Status, Status_Value);
      return Status_Value /= 0;
   end Validate;

   function Info_Log (Object : Pipeline) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Program_Pipeline_Param.Ref
        (Object.Reference.GL_Id, Enums.Info_Log_Length, Log_Length);

      if Log_Length = 0 then
         return "";
      end if;

      declare
         Info_Log : String (1 .. Integer (Log_Length));
      begin
         API.Get_Program_Pipeline_Info_Log.Ref
           (Object.Reference.GL_Id, Log_Length, Log_Length, Info_Log);
         return Info_Log (1 .. Integer (Log_Length));
      end;
   end Info_Log;

   overriding
   procedure Initialize_Id (Object : in out Pipeline) is
      New_Id : UInt := 0;
   begin
      API.Create_Program_Pipelines.Ref (1, New_Id);
      Object.Reference.GL_Id := New_Id;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Pipeline) is
   begin
      API.Delete_Program_Pipelines.Ref (1, [Object.Reference.GL_Id]);
      Object.Reference.GL_Id := 0;
   end Delete_Id;

end GL.Objects.Pipelines;
