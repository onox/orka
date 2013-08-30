--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with GL.Attributes;
with GL.Objects.Shaders;
with GL.Uniforms;

package GL.Objects.Programs is
   type Program is new GL_Object with private;
      
   procedure Attach (Subject : Program; Shader : Shaders.Shader);
   
   procedure Link (Subject : Program);
   
   function Link_Status (Subject : Program) return Boolean;
   
   function Info_Log (Subject : Program) return String;
   
   procedure Use_Program (Subject : Program);
   
   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform;
   
   procedure Bind_Attrib_Location (Subject : Program;
                                   Index : Attributes.Attribute;
                                   Name : String);
   function Attrib_Location (Subject : Program; Name : String)
     return Attributes.Attribute;
   
   overriding
   procedure Initialize_Id (Object : in out Program);
   
   overriding
   procedure Delete_Id (Object : in out Program);
private
   type Program is new GL_Object with null record;
end GL.Objects.Programs;