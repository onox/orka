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

with GL.Objects.Buffers;

with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Programs.Uniforms;

package body Orka.Rendering.Programs is

   function Create_Program (Modules   : Programs.Modules.Module_Array;
                            Separable : Boolean := False) return Program is
      use type GL.Types.Int;
   begin
      return Result : Program do
         Result.GL_Program.Set_Separable (Separable);

         --  Attach all shaders to the program before linking
         Programs.Modules.Attach_Shaders (Modules, Result);

         Result.GL_Program.Link;
         Programs.Modules.Detach_Shaders (Modules, Result);

         if not Result.GL_Program.Link_Status then
            raise Program_Link_Error with Result.GL_Program.Info_Log;
         end if;

         --  Construct arrays of subroutine indices per shader kind
         Result.Has_Subroutines := False;
         Result.Subroutines_Modified := False;

         for Shader_Kind in Result.Stages'Range loop
            if Result.Stages (Shader_Kind) then
               declare
                  Locations : constant GL.Types.Size
                    := Result.GL_Program.Subroutine_Uniform_Locations (Shader_Kind);
                  subtype Indices_Array is GL.Types.UInt_Array (0 .. Locations - 1);
               begin
                  if Indices_Array'Length > 0 then
                     Result.Has_Subroutines := True;
                     Result.Subroutines (Shader_Kind)
                       := Subroutines_Holder.To_Holder
                            (Indices_Array'(others => GL.Types.UInt'Last));
                  end if;
               end;
            end if;
         end loop;
      end return;
   end Create_Program;

   function Create_Program (Module    : Programs.Modules.Module;
                            Separable : Boolean := False) return Program is
   begin
      return Create_Program (Modules.Module_Array'(1 => Module), Separable);
   end Create_Program;

   function Has_Subroutines (Object : Program) return Boolean
     is (Object.Has_Subroutines);

   procedure Use_Subroutines (Object : in out Program) is
   begin
      for Shader_Kind in Object.Subroutines'Range loop
         if not Object.Subroutines (Shader_Kind).Is_Empty then
            declare
               Indices : GL.Types.UInt_Array renames Object.Subroutines (Shader_Kind).Element;
            begin
               pragma Assert (Indices'Length > 0);
               GL.Objects.Programs.Set_Uniform_Subroutines (Shader_Kind, Indices);
            end;
         end if;
      end loop;
      Object.Subroutines_Modified := False;
   end Use_Subroutines;

   procedure Use_Program (Object : in out Program) is
   begin
      Object.GL_Program.Use_Program;
      if Object.Has_Subroutines then
         Object.Use_Subroutines;
      end if;
   end Use_Program;

   function Compute_Work_Group_Size
     (Object : Program) return GL.Types.Compute.Dimension_Size_Array
   is (Object.GL_Program.Compute_Work_Group_Size);

   procedure Set_Subroutine_Function
     (Object   : in out Program;
      Shader   : GL.Objects.Shaders.Shader_Type;
      Location : Uniform_Location;
      Index    : Subroutine_Index)
   is
      procedure Set_Index (Indices : in out GL.Types.UInt_Array) is
      begin
         Indices (Location) := Index;
      end Set_Index;
   begin
      Object.Subroutines (Shader).Update_Element (Set_Index'Access);
      Object.Subroutines_Modified := True;
   end Set_Subroutine_Function;

   function Uniform_Sampler (Object : Program; Name : String)
     return Uniforms.Uniform_Sampler is
   begin
      return Uniforms.Create_Uniform_Sampler (Object, Name);
   end Uniform_Sampler;

   function Uniform_Image (Object : Program; Name : String)
     return Uniforms.Uniform_Image is
   begin
      return Uniforms.Create_Uniform_Image (Object, Name);
   end Uniform_Image;

   function Uniform_Subroutine
     (Object : in out Program;
      Shader : GL.Objects.Shaders.Shader_Type;
      Name   : String) return Uniforms.Uniform_Subroutine is
   begin
      return Uniforms.Create_Uniform_Subroutine (Object, Shader, Name);
   end Uniform_Subroutine;

   function Uniform (Object : Program; Name : String)
     return Uniforms.Uniform is
   begin
      return Uniforms.Create_Uniform_Variable (Object, Name);
   end Uniform;

   function Binding
     (Object : Program;
      Target : Buffers.Indexed_Buffer_Target;
      Name   : String) return Natural
   is
      use all type Buffers.Indexed_Buffer_Target;
   begin
      return Natural (Object.GL_Program.Buffer_Binding
        ((case Target is
           when Uniform        => GL.Objects.Buffers.Uniform,
           when Shader_Storage => GL.Objects.Buffers.Shader_Storage,
           when Atomic_Counter => GL.Objects.Buffers.Atomic_Counter),
         Name));
   end Binding;

end Orka.Rendering.Programs;
