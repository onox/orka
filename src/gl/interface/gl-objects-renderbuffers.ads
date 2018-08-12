--  Copyright (c) 2013 Felix Krause <contact@flyx.org>
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

with GL.Pixels;

with GL.Low_Level.Enums;

package GL.Objects.Renderbuffers is
   pragma Preelaborate;

   type Renderbuffer_Target (<>) is tagged limited private;

   function Raw_Kind (Object : Renderbuffer_Target)
                      return Low_Level.Enums.Renderbuffer_Kind;

   Active_Renderbuffer : constant Renderbuffer_Target;

   type Renderbuffer is new GL_Object with private;

   procedure Bind (Target : Renderbuffer_Target; Object : Renderbuffer'Class);

   function Current (Target : Renderbuffer_Target) return Renderbuffer'Class;

   procedure Allocate (Object : Renderbuffer;
                       Format : Pixels.Internal_Format;
                       Width, Height : Size;
                       Samples : Size := 0);

   function Width  (Object : Renderbuffer) return Size;
   function Height (Object : Renderbuffer) return Size;
   function Internal_Format (Object : Renderbuffer)
                             return Pixels.Internal_Format;
   function Red_Size     (Object : Renderbuffer) return Size;
   function Green_Size   (Object : Renderbuffer) return Size;
   function Blue_Size    (Object : Renderbuffer) return Size;
   function Alpha_Size   (Object : Renderbuffer) return Size;
   function Depth_Size   (Object : Renderbuffer) return Size;
   function Stencil_Size (Object : Renderbuffer) return Size;
   function Samples      (Object : Renderbuffer) return Size;

   overriding
   procedure Initialize_Id (Object : in out Renderbuffer);

   overriding
   procedure Delete_Id (Object : in out Renderbuffer);

   overriding
   function Identifier (Object : Renderbuffer) return Types.Debug.Identifier is
     (Types.Debug.Renderbuffer);

   No_Renderbuffer : constant Renderbuffer;

private

   type Renderbuffer is new GL_Object with null record;

   type Renderbuffer_Target (Kind : Low_Level.Enums.Renderbuffer_Kind) is
     tagged limited null record;

   Active_Renderbuffer : constant Renderbuffer_Target
     := Renderbuffer_Target'(Kind => Low_Level.Enums.Renderbuffer);

   No_Renderbuffer : constant Renderbuffer :=
     Renderbuffer'(GL_Object with null record);

end GL.Objects.Renderbuffers;
