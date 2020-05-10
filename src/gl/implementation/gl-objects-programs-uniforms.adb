--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.API.Uniforms.Singles;
with GL.API.Uniforms.Doubles;
with GL.API.Uniforms.Ints;
with GL.API.Uniforms.UInts;
with GL.Low_Level;

package body GL.Objects.Programs.Uniforms is

   function Create_Uniform (Object : Program; Location : Int) return Uniform is
   begin
      return Uniform'(Program => Object, Location => Location);
   end Create_Uniform;

   -----------------------------------------------------------------------------
   --                                 Singles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Single (Location : Uniform; Value : Single) is
   begin
      API.Uniforms.Singles.Uniform1.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value);
   end Set_Single;

   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector2) is
   begin
      API.Uniforms.Singles.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Single_Vector;

   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector3) is
   begin
      API.Uniforms.Singles.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Single_Vector;

   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector4) is
   begin
      API.Uniforms.Singles.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Single_Vector;

   procedure Set_Single_Array (Location : Uniform; Value : Single_Array) is
   begin
      API.Uniforms.Singles.Uniform1v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Single_Array;

   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector2_Array) is
   begin
      API.Uniforms.Singles.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Single_Vectors;

   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector3_Array) is
   begin
      API.Uniforms.Singles.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Single_Vectors;

   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector4_Array) is
   begin
      API.Uniforms.Singles.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Single_Vectors;

   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix2) is
   begin
      API.Uniforms.Singles.Uniform_Matrix2.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Single_Matrix;

   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix3) is
   begin
      API.Uniforms.Singles.Uniform_Matrix3.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Single_Matrix;

   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix4) is
   begin
      API.Uniforms.Singles.Uniform_Matrix4.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Single_Matrix;

   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix2_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix2.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Single_Matrices;

   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix3_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix3.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Single_Matrices;

   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix4_Array) is
   begin
      API.Uniforms.Singles.Uniform_Matrix4.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Single_Matrices;

   -----------------------------------------------------------------------------
   --                                 Doubles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Double (Location : Uniform; Value : Double) is
   begin
      API.Uniforms.Doubles.Uniform1.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value);
   end Set_Double;

   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector2) is
   begin
      API.Uniforms.Doubles.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Double_Vector;

   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector3) is
   begin
      API.Uniforms.Doubles.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Double_Vector;

   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector4) is
   begin
      API.Uniforms.Doubles.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Double_Vector;

   procedure Set_Double_Array (Location : Uniform; Value : Double_Array) is
   begin
      API.Uniforms.Doubles.Uniform1v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Double_Array;

   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector2_Array) is
   begin
      API.Uniforms.Doubles.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Double_Vectors;

   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector3_Array) is
   begin
      API.Uniforms.Doubles.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Double_Vectors;

   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector4_Array) is
   begin
      API.Uniforms.Doubles.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Double_Vectors;

   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix2) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix2.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Double_Matrix;

   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix3) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix3.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Double_Matrix;

   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix4) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix4.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         1, Low_Level.False, (1 => Value));
   end Set_Double_Matrix;

   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix2_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix2.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Double_Matrices;

   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix3_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix3.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Double_Matrices;

   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix4_Array) is
   begin
      API.Uniforms.Doubles.Uniform_Matrix4.Ref
        (Location.Program.Reference.GL_Id, Location.Location,
         Value'Length, Low_Level.False, Value);
   end Set_Double_Matrices;

   -----------------------------------------------------------------------------
   --                                 Integers                                --
   -----------------------------------------------------------------------------

   procedure Set_Int (Location : Uniform; Value : Int) is
   begin
      API.Uniforms.Ints.Uniform1.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value);
   end Set_Int;

   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector2) is
   begin
      API.Uniforms.Ints.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Int_Vector;

   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector3) is
   begin
      API.Uniforms.Ints.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Int_Vector;

   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector4) is
   begin
      API.Uniforms.Ints.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_Int_Vector;

   procedure Set_Int_Array (Location : Uniform; Value : Int_Array) is
   begin
      API.Uniforms.Ints.Uniform1v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Int_Array;

   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector2_Array) is
   begin
      API.Uniforms.Ints.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Int_Vectors;

   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector3_Array) is
   begin
      API.Uniforms.Ints.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Int_Vectors;

   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector4_Array) is
   begin
      API.Uniforms.Ints.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_Int_Vectors;

   -----------------------------------------------------------------------------
   --                            Unsigned Integers                            --
   -----------------------------------------------------------------------------

   procedure Set_UInt (Location : Uniform; Value : UInt) is
   begin
      API.Uniforms.UInts.Uniform1.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value);
   end Set_UInt;

   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector2) is
   begin
      API.Uniforms.UInts.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_UInt_Vector;

   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector3) is
   begin
      API.Uniforms.UInts.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_UInt_Vector;

   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector4) is
   begin
      API.Uniforms.UInts.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, 1, (1 => Value));
   end Set_UInt_Vector;

   procedure Set_UInt_Array (Location : Uniform; Value : UInt_Array) is
   begin
      API.Uniforms.UInts.Uniform1v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_UInt_Array;

   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector2_Array) is
   begin
      API.Uniforms.UInts.Uniform2v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_UInt_Vectors;

   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector3_Array) is
   begin
      API.Uniforms.UInts.Uniform3v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_UInt_Vectors;

   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector4_Array) is
   begin
      API.Uniforms.UInts.Uniform4v.Ref
        (Location.Program.Reference.GL_Id, Location.Location, Value'Length, Value);
   end Set_UInt_Vectors;

end GL.Objects.Programs.Uniforms;
