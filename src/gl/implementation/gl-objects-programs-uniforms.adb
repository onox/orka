--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with GL.API.Uniforms.Singles;
with GL.API.Uniforms.Doubles;
with GL.API.Uniforms.Ints;
with GL.API.Uniforms.UInts;
with GL.Low_Level;

package body GL.Objects.Programs.Uniforms is

   -----------------------------------------------------------------------------
   --                                 Singles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Single (Location : Uniform; Value : Single) is
   begin
      API.Uniforms.Singles.Uniform1 (Location.Program.Reference.GL_Id,
                                     Location.Location, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2 : Single) is
   begin
      API.Uniforms.Singles.Uniform2 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector2) is
   begin
      API.Uniforms.Singles.Uniform2v (Location.Program.Reference.GL_Id,
                                      Location.Location, 2, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2, V3 : Single) is
   begin
      API.Uniforms.Singles.Uniform3 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2, V3);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector3) is
   begin
      API.Uniforms.Singles.Uniform3v (Location.Program.Reference.GL_Id,
                                      Location.Location, 3, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; V1, V2, V3, V4 : Single) is
   begin
      API.Uniforms.Singles.Uniform4 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2, V3, V4);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector4) is
   begin
      API.Uniforms.Singles.Uniform4v (Location.Program.Reference.GL_Id,
                                      Location.Location, 4, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Single_Array) is
   begin
      API.Uniforms.Singles.Uniform1v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector2_Array) is
   begin
      API.Uniforms.Singles.Uniform2v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector3_Array) is
   begin
      API.Uniforms.Singles.Uniform3v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Vector4_Array) is
   begin
      API.Uniforms.Singles.Uniform4v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2) is
   begin
      API.Uniforms.Singles.Uniform_Matrix2 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3) is
   begin
      API.Uniforms.Singles.Uniform_Matrix3 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4) is
   begin
      API.Uniforms.Singles.Uniform_Matrix4 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix2_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix2 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix3_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix3 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Single;

   procedure Set_Single (Location : Uniform; Value : Singles.Matrix4_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix4 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Single;

   -----------------------------------------------------------------------------
   --                                 Doubles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Double (Location : Uniform; Value : Double) is
   begin
      API.Uniforms.Doubles.Uniform1 (Location.Program.Reference.GL_Id,
                                     Location.Location, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; V1, V2 : Double) is
   begin
      API.Uniforms.Doubles.Uniform2 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector2) is
   begin
      API.Uniforms.Doubles.Uniform2v (Location.Program.Reference.GL_Id,
                                      Location.Location, 2, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; V1, V2, V3 : Double) is
   begin
      API.Uniforms.Doubles.Uniform3 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2, V3);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector3) is
   begin
      API.Uniforms.Doubles.Uniform3v (Location.Program.Reference.GL_Id,
                                      Location.Location, 3, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; V1, V2, V3, V4 : Double) is
   begin
      API.Uniforms.Doubles.Uniform4 (Location.Program.Reference.GL_Id,
                                     Location.Location, V1, V2, V3, V4);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector4) is
   begin
      API.Uniforms.Doubles.Uniform4v (Location.Program.Reference.GL_Id,
                                      Location.Location, 4, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Double_Array) is
   begin
      API.Uniforms.Doubles.Uniform1v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector2_Array) is
   begin
      API.Uniforms.Doubles.Uniform2v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector3_Array) is
   begin
      API.Uniforms.Doubles.Uniform3v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Vector4_Array) is
   begin
      API.Uniforms.Doubles.Uniform4v (Location.Program.Reference.GL_Id,
                                      Location.Location, Value'Length, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix2) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix2 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix3) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix3 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix4) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix4 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            1, Low_Level.False, (1 => Value));
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix2_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix2 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix3_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix3 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Double;

   procedure Set_Double (Location : Uniform; Value : Doubles.Matrix4_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix4 (Location.Program.Reference.GL_Id,
                                            Location.Location,
                                            Value'Length, Low_Level.False, Value);
   end Set_Double;

   -----------------------------------------------------------------------------
   --                                 Integers                                --
   -----------------------------------------------------------------------------

   procedure Set_Int (Location : Uniform; Value : Int) is
   begin
      API.Uniforms.Ints.Uniform1 (Location.Program.Reference.GL_Id,
                                  Location.Location, Value);
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2 : Int) is
   begin
      API.Uniforms.Ints.Uniform2 (Location.Program.Reference.GL_Id,
                                  Location.Location, V1, V2);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector2) is
   begin
      API.Uniforms.Ints.Uniform2v (Location.Program.Reference.GL_Id,
                                   Location.Location, 2, (1 => Value));
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2, V3 : Int) is
   begin
      API.Uniforms.Ints.Uniform3 (Location.Program.Reference.GL_Id,
                                  Location.Location, V1, V2, V3);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector3) is
   begin
      API.Uniforms.Ints.Uniform3v (Location.Program.Reference.GL_Id,
                                   Location.Location, 3, (1 => Value));
   end Set_Int;

   procedure Set_Int (Location : Uniform; V1, V2, V3, V4 : Int) is
   begin
      API.Uniforms.Ints.Uniform4 (Location.Program.Reference.GL_Id,
                                  Location.Location, V1, V2, V3, V4);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector4) is
   begin
      API.Uniforms.Ints.Uniform4v (Location.Program.Reference.GL_Id,
                                   Location.Location, 4, (1 => Value));
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Int_Array) is
   begin
      API.Uniforms.Ints.Uniform1v (Location.Program.Reference.GL_Id,
                                   Location.Location, Value'Length, Value);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector2_Array) is
   begin
      API.Uniforms.Ints.Uniform2v (Location.Program.Reference.GL_Id,
                                   Location.Location, Value'Length * 2, Value);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector3_Array) is
   begin
      API.Uniforms.Ints.Uniform3v (Location.Program.Reference.GL_Id,
                                   Location.Location, Value'Length * 3, Value);
   end Set_Int;

   procedure Set_Int (Location : Uniform; Value : Ints.Vector4_Array) is
   begin
      API.Uniforms.Ints.Uniform4v (Location.Program.Reference.GL_Id,
                                   Location.Location, Value'Length * 4, Value);
   end Set_Int;

   -----------------------------------------------------------------------------
   --                            Unsigned Integers                            --
   -----------------------------------------------------------------------------

   procedure Set_UInt (Location : Uniform; Value : UInt) is
   begin
      API.Uniforms.UInts.Uniform1 (Location.Program.Reference.GL_Id,
                                   Location.Location, Value);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2 : UInt) is
   begin
      API.Uniforms.UInts.Uniform2 (Location.Program.Reference.GL_Id,
                                   Location.Location, V1, V2);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2) is
   begin
      API.Uniforms.UInts.Uniform2v (Location.Program.Reference.GL_Id,
                                    Location.Location, 2, (1 => Value));
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2, V3 : UInt) is
   begin
      API.Uniforms.UInts.Uniform3 (Location.Program.Reference.GL_Id,
                                   Location.Location, V1, V2, V3);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3) is
   begin
      API.Uniforms.UInts.Uniform3v (Location.Program.Reference.GL_Id,
                                    Location.Location, 3, (1 => Value));
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; V1, V2, V3, V4 : UInt) is
   begin
      API.Uniforms.UInts.Uniform4 (Location.Program.Reference.GL_Id,
                                   Location.Location, V1, V2, V3, V4);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4) is
   begin
      API.Uniforms.UInts.Uniform4v (Location.Program.Reference.GL_Id,
                                    Location.Location, 4, (1 => Value));
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInt_Array) is
   begin
      API.Uniforms.UInts.Uniform1v (Location.Program.Reference.GL_Id,
                                    Location.Location, Value'Length, Value);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector2_Array) is
   begin
      API.Uniforms.UInts.Uniform2v (Location.Program.Reference.GL_Id,
                                    Location.Location, Value'Length, Value);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector3_Array) is
   begin
      API.Uniforms.UInts.Uniform3v (Location.Program.Reference.GL_Id,
                                    Location.Location, Value'Length, Value);
   end Set_UInt;

   procedure Set_UInt (Location : Uniform; Value : UInts.Vector4_Array) is
   begin
      API.Uniforms.UInts.Uniform4v (Location.Program.Reference.GL_Id,
                                    Location.Location, Value'Length, Value);
   end Set_UInt;

end GL.Objects.Programs.Uniforms;