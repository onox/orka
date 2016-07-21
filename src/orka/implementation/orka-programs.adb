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

with Orka.Programs.Modules;

package body Orka.Programs is

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program is
   begin
      return Result : Program do
         Result.GL_Program.Initialize_Id;

         --  Attach all shaders to the program before linking
         Programs.Modules.Attach_Shaders (Modules, Result);

         Result.GL_Program.Link;
         if not Result.GL_Program.Link_Status then
            raise Program_Link_Error with Result.GL_Program.Info_Log;
         end if;
      end return;
   end Create_Program;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program is
   begin
      return Create_Program ((1 => Module));
   end Create_Program;

   function GL_Program (Object : Program) return GL.Objects.Programs.Program
     is (Object.GL_Program);

   procedure Use_Program (Object : Program) is
   begin
      Object.GL_Program.Use_Program;
      --  TODO Only call if current program is a different object
   end Use_Program;

   function Attribute_Location (Object : Program; Name : String)
      return GL.Attributes.Attribute is
   begin
      return Object.GL_Program.Attrib_Location (Name);
   end Attribute_Location;

end Orka.Programs;
