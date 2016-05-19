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

package GL.Objects.Programs.Uniforms is
   pragma Preelaborate;

   type Uniform is tagged private;

   function Create_Uniform (Object : Program; Location : Int) return Uniform
     with Inline;

   -----------------------------------------------------------------------------
   --                                 Singles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Single  (Location : Uniform; Value          : Single);
   procedure Set_Singles (Location : Uniform; V1, V2         : Single);
   procedure Set_Singles (Location : Uniform; V1, V2, V3     : Single);
   procedure Set_Singles (Location : Uniform; V1, V2, V3, V4 : Single);

   procedure Set_Single_Array (Location : Uniform; Value : Single_Array);

   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector2);
   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector3);
   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector4);

   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector2_Array);
   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector3_Array);
   procedure Set_Single_Vectors (Location : Uniform; Value : Singles.Vector4_Array);

   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix2);
   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix3);
   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix4);

   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix2_Array);
   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix3_Array);
   procedure Set_Single_Matrices (Location : Uniform; Value : Singles.Matrix4_Array);

   -----------------------------------------------------------------------------
   --                                 Doubles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Double  (Location : Uniform; Value          : Double);
   procedure Set_Doubles (Location : Uniform; V1, V2         : Double);
   procedure Set_Doubles (Location : Uniform; V1, V2, V3     : Double);
   procedure Set_Doubles (Location : Uniform; V1, V2, V3, V4 : Double);

   procedure Set_Double_Array (Location : Uniform; Value : Double_Array);

   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector2);
   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector3);
   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector4);

   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector2_Array);
   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector3_Array);
   procedure Set_Double_Vectors (Location : Uniform; Value : Doubles.Vector4_Array);

   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix2);
   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix3);
   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix4);

   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix2_Array);
   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix3_Array);
   procedure Set_Double_Matrices (Location : Uniform; Value : Doubles.Matrix4_Array);

   -----------------------------------------------------------------------------
   --                                 Integers                                --
   -----------------------------------------------------------------------------

   procedure Set_Int  (Location : Uniform; Value          : Int);
   procedure Set_Ints (Location : Uniform; V1, V2         : Int);
   procedure Set_Ints (Location : Uniform; V1, V2, V3     : Int);
   procedure Set_Ints (Location : Uniform; V1, V2, V3, V4 : Int);

   procedure Set_Int_Array (Location : Uniform; Value : Int_Array);

   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector2);
   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector3);
   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector4);

   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector2_Array);
   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector3_Array);
   procedure Set_Int_Vectors (Location : Uniform; Value : Ints.Vector4_Array);

   -----------------------------------------------------------------------------
   --                            Unsigned Integers                            --
   -----------------------------------------------------------------------------

   procedure Set_UInt  (Location : Uniform; Value          : UInt);
   procedure Set_UInts (Location : Uniform; V1, V2         : UInt);
   procedure Set_UInts (Location : Uniform; V1, V2, V3     : UInt);
   procedure Set_UInts (Location : Uniform; V1, V2, V3, V4 : UInt);

   procedure Set_UInt_Array (Location : Uniform; Value : UInt_Array);

   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector2);
   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector3);
   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector4);

   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector2_Array);
   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector3_Array);
   procedure Set_UInt_Vectors (Location : Uniform; Value : UInts.Vector4_Array);

private

   type Uniform is tagged record
      Program  : Programs.Program;
      Location : Int;
   end record;

end GL.Objects.Programs.Uniforms;
