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

package GL.Objects.Programs.Uniforms is
   pragma Preelaborate;

   type Uniform is tagged private;

   function Create_Uniform (Object : Program; Location : Int) return Uniform
     with Inline;

   -----------------------------------------------------------------------------
   --                                 Singles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Single  (Location : Uniform; Value : Single);

   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector2);
   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector3);
   procedure Set_Single_Vector (Location : Uniform; Value : Singles.Vector4);

   procedure Set_Single_Matrix (Location : Uniform; Value : Singles.Matrix4);

   -----------------------------------------------------------------------------
   --                                 Doubles                                 --
   -----------------------------------------------------------------------------

   procedure Set_Double  (Location : Uniform; Value : Double);

   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector2);
   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector3);
   procedure Set_Double_Vector (Location : Uniform; Value : Doubles.Vector4);

   procedure Set_Double_Matrix (Location : Uniform; Value : Doubles.Matrix4);

   -----------------------------------------------------------------------------
   --                                 Integers                                --
   -----------------------------------------------------------------------------

   procedure Set_Int  (Location : Uniform; Value : Int);

   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector2);
   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector3);
   procedure Set_Int_Vector (Location : Uniform; Value : Ints.Vector4);

   -----------------------------------------------------------------------------
   --                            Unsigned Integers                            --
   -----------------------------------------------------------------------------

   procedure Set_UInt  (Location : Uniform; Value : UInt);

   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector2);
   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector3);
   procedure Set_UInt_Vector (Location : Uniform; Value : UInts.Vector4);

private

   type Uniform is tagged record
      Program  : Programs.Program;
      Location : Int;
   end record;

end GL.Objects.Programs.Uniforms;
