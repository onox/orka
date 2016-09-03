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

package body GL.Matrices is      

   function "+" (Left, Right : Matrix) return Matrix is
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Return_Matrix (I, J) := Left (I, J) + Right (I, J);
         end loop;
      end loop;
      return Return_Matrix;
   end "+";
   
   function "-" (Left, Right : Matrix) return Matrix is
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Return_Matrix (I, J) := Left (I, J) - Right (I, J);
         end loop;
      end loop;
      return Return_Matrix;
   end "-";
   
   function "-" (Left : Matrix) return Matrix is
      Ret : Matrix;
   begin
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Ret (I, J) := -Left (I, J);
         end loop;
      end loop;
      return Ret;
   end "-";
   
   function "*" (Left, Right : Matrix) return Matrix is
      Element : Element_Type;
      Return_Matrix : Matrix;
   begin
      --  I is the column index and J is the row index
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Element := Null_Value;
            for X in Index_Type loop
               pragma Loop_Optimize (Unroll);
               Element := Element + Left (X, J) * Right (I, X);
            end loop;
            Return_Matrix (I, J) := Element;
         end loop;
      end loop;
      return Return_Matrix;
   end "*";
   
   function "*" (Left : Matrix; Right : Vector_Type) return Vector_Type is
      Element : Element_Type;
      Return_Vector : Vector_Type;
   begin
      --  J is the row index
      for J in Index_Type loop
         pragma Loop_Optimize (Unroll);
         Element := Null_Value;
         for X in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Element := Element + Left (X, J) * Right (X);
         end loop;
         Return_Vector (J) := Element;
      end loop;
      return Return_Vector;
   end "*";
   
   function "*" (Left : Matrix; Right : Element_Type) return Matrix is
      Return_Matrix : Matrix;
   begin
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Return_Matrix (I, J) := Left (I, J) * Right;
         end loop;
      end loop;
      return Return_Matrix;
   end "*";
   
   function "*" (Left : Element_Type; Right : Matrix) return Matrix is
   begin
      return Right * Left;
   end "*";
   
   function Transpose (Subject : Matrix) return Matrix is
      Ret : Matrix;
   begin
      for I in Index_Type loop
         pragma Loop_Optimize (Unroll);
         for J in Index_Type loop
            pragma Loop_Optimize (Unroll);
            Ret (I, J) := Subject (J, I);
         end loop;
      end loop;
      return Ret;
   end Transpose;

end GL.Matrices;
