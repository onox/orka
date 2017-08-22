--  Copyright (c) 2012 Felix Krause <contact@flyx.org>
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

with Ada.Unchecked_Deallocation;

package body GL.Objects is

   overriding procedure Initialize (Object : in out GL_Object) is
   begin
      Object.Reference := new GL_Object_Reference'(GL_Id          => 0,
                                                  Reference_Count => 1,
                                                  Initialized     => False);
      GL_Object'Class (Object).Initialize_Id;
   end Initialize;

   overriding procedure Adjust (Object : in out GL_Object) is
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out GL_Object) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => GL_Object_Reference, Name => GL_Object_Reference_Access);
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count - 1;
         if Object.Reference.Reference_Count = 0 then
            if Object.Reference.Initialized then
               GL_Object'Class (Object).Delete_Id;
            end if;
            Free (Object.Reference);
         end if;
      end if;

      --  Idempotence: next call to Finalize has no effect
      Object.Reference := null;
   end Finalize;
   
   function Initialized (Object : GL_Object) return Boolean is
   begin
      return Object.Reference.Initialized;
   end Initialized;

   function Raw_Id (Object : GL_Object) return UInt is
   begin
      return Object.Reference.GL_Id;
   end Raw_Id;
   
   procedure Set_Raw_Id (Object : GL_Object; Id : UInt) is
   begin
      Object.Reference.GL_Id := Id;
   end Set_Raw_Id;

   overriding
   function "=" (Left, Right : GL_Object) return Boolean is
   begin
      return Left.Reference = Right.Reference;
   end "=";

end GL.Objects;
