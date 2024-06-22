--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
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

package body Orka.Numerics.Tensors is

   package body Generic_Random is

      --  Box-Muller method for the standard normal distribution
      --
      --  Alternatively, (U_1 + ... + U_12) - 6.0 uses the central limit
      --  theorem to approximate the standard normal distribution with
      --  samples limited to (-6, 6). 0.00034 % of the actual standard normal
      --  distribution falls outside this range.
      function Normal (Shape : Tensor_Shape) return Random_Tensor is
        (Multiply (Sqrt (Convert (-2.0) * Log (Uniform (Shape) + Element_Model_Small)),
           Cos (Convert (2.0 * Ada.Numerics.Pi) * Uniform (Shape))));

      function Binomial
        (Shape : Tensor_Shape;
         N     : Positive;
         P     : Probability) return Random_Tensor
      is
         Result : Random_Tensor := Zeros (Shape);
      begin
         for Index in 1 .. N loop
            Result := Result + (One and (Uniform (Shape) <= Convert (Real_Element (P))));
         end loop;

         return Result;
      end Binomial;

      function Geometric (Shape : Tensor_Shape; P : Probability) return Random_Tensor is
        (Floor (Exponential (Shape, -EF.Log (Real_Element (1.0 - P) + Real_Element'Model_Small))));

      function Poisson (Shape : Tensor_Shape; Lambda : Real_Element) return Random_Tensor is
         U : constant Random_Tensor := Uniform (Shape);

         use EF;

         I    : Random_Tensor := Zeros (Shape);
         Expr : Random_Tensor := Fill (Shape, Convert (Ada.Numerics.e ** (-Lambda)));
         Sum  : Random_Tensor := Expr;
      begin
         loop
            declare
               Loop_Condition : constant Random_Tensor := U > Sum;
            begin
               exit when not Any_True (Loop_Condition);

               --  Using inverse transform sampling (with y is lambda):
               --  P = min{p = 0, 1, 2, ... | U <= exp(-y) * Sum (y^i / i! for i in 0 .. p)}
               I    := I + (One and Loop_Condition);
               Expr := Expr * Convert (Lambda) / I;
               Sum  := Sum + (Expr and Loop_Condition);
            end;
         end loop;

         return I;
      end Poisson;

      function Gamma (Shape : Tensor_Shape; K, Theta : Real_Element) return Random_Tensor is
         Float_D : constant Real_Element := K - 1.0 / 3.0;
         Float_C : constant Real_Element := 1.0 / EF.Sqrt (9.0 * Float_D);

         D : constant Element_Type := Convert (Float_D);
         C : constant Element_Type := Convert (Float_C);

         Result   : Random_Tensor := Zeros (Shape);
         Fraction : Random_Tensor := Zeros (Shape);
      begin
         for I in 1 .. Integer (Real_Element'Floor (K)) loop
            Result := Result + Log (Uniform (Shape) + Element_Model_Small);
            Result.Materialize;
         end loop;

         --  Marsaglia's transformation-rejection method [1]
         --
         --  [1] "A Simple Method for Generating Gamma Variables", Marsaglia G., Tsang W.,
         --      ACM Trans. Math. Softw., 26.3:363−372, 2000
         --
         --  See https://en.wikipedia.org/wiki/Gamma_distribution
         declare
            Loop_Condition : Random_Tensor := Fraction /= Fraction;
         begin
            loop
               declare
                  X : constant Random_Tensor := Normal (Shape);
                  U : constant Random_Tensor := Log (Uniform (Shape) + Element_Model_Small);

                  V  : constant Random_Tensor := Power (One + C * X, 3);
                  DV : constant Random_Tensor := D * V;

                  --  (Check that -1.0 / C < X, otherwise Log (V) will fail because V <= 0.0)
                  If_Condition : constant Random_Tensor :=
                    V > Zero and Convert (-1.0 / Float_C) < X and
                      U < Convert (0.5) * Power (X, 2) + D - DV
                            + D * Log (Max (Zero, V) + Element_Model_Small);
               begin
                  --  Update Fraction with value DV but not if the element was already true before
                  Fraction := Fraction + DV and And_Not (Loop_Condition, If_Condition);

                  Loop_Condition := Loop_Condition or If_Condition;
                  exit when All_True (Loop_Condition);
               end;
            end loop;
         end;

         return Convert (Theta) * (Fraction - Result);
      end Gamma;

      --  Beta (Alpha, Beta) = X / (X + Y) where X ~ Gamma(Alpha, Theta) and Y ~ Gamma(Beta, Theta)
      function Beta (Shape : Tensor_Shape; Alpha, Beta : Real_Element) return Random_Tensor is
         X : constant Random_Tensor := Gamma (Shape, Alpha, 1.0);
         Y : constant Random_Tensor := Gamma (Shape, Beta, 1.0);
      begin
         return X / (X + Y);
      end Beta;

      function Test_Statistic_T_Test (Data : Random_Tensor; True_Mean : Element) return Element is
         Sample_Mean : constant Real_Element := Convert (Data.Mean);
         Sample_Std  : constant Real_Element := Convert (Data.Standard_Deviation (Offset => 1));
      begin
         return Convert ((Sample_Mean - Convert (True_Mean)) / (Sample_Std / EF.Sqrt (Real_Element (Data.Elements))));
      end Test_Statistic_T_Test;

      function Threshold_T_Test
        (Data  : Random_Tensor;
         Level : Probability) return Element
      is
         Sample_Std : constant Real_Element := Convert (Data.Standard_Deviation (Offset => 1));

         use EF;

         --  Simple approximation by Shore (1982)
--         T_Value : constant Element := 5.5556 * (1.0 - Element (Level / (1.0 - Level))**0.1186);

         --  "A handy approximation for the error function and its inverse", Winitzki S., 2008,
         --  equation 7.
         function Inverse_Erf (X : Probability) return Real_Element is
            A        : constant := 0.147;
            Two_Pi_A : constant := 2.0 / (Ada.Numerics.Pi * A);

            Ln_X2          : constant Real_Element := Log (1.0 - Real_Element (X)**2);
            Two_Pi_A_Ln_X2 : constant Real_Element := Two_Pi_A + Ln_X2 / 2.0;
         begin
            return Sqrt (Sqrt (Two_Pi_A_Ln_X2**2 - Ln_X2 / A) - Two_Pi_A_Ln_X2);
         end Inverse_Erf;

         --  Quantile function of standard normal distribution
         T_Value : constant Real_Element := Sqrt (2.0) * Inverse_Erf (2.0 * (1.0 - Level) - 1.0);
      begin
         return Convert (T_Value * (Sample_Std / EF.Sqrt (Real_Element (Data.Elements))));
      end Threshold_T_Test;

   end Generic_Random;

   ----------------------------------------------------------------------------
   --                              Expressions                               --
   ----------------------------------------------------------------------------

   overriding
   function "+" (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Add,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "-" (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Subtract,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "*" (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Multiply,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "/" (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Divide,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function Min (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Min,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function Max (Left, Right : Expression_Type) return Expression_Type is
     (Kind     => Binary_Operation,
      Operator => Max,
      Left     => Expression_Holders.To_Holder (Left),
      Right    => Expression_Holders.To_Holder (Right));

   overriding
   function "-" (Value : Expression_Type) return Expression_Type is
     (Kind           => Unary_Operation,
      Unary_Operator => Minus,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding
   function "abs" (Value : Expression_Type) return Expression_Type is
     (Kind           => Unary_Operation,
      Unary_Operator => Absolute,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding
   function Sqrt (Value : Expression_Type) return Expression_Type is
     (Kind           => Unary_Operation,
      Unary_Operator => Sqrt,
      Expression     => Expression_Holders.To_Holder (Value));

   overriding function X return Expression_Type is (Kind => Argument, Argument => X);
   overriding function Y return Expression_Type is (Kind => Argument, Argument => Y);

   overriding function Number (Value : Element) return Expression_Type is
     (Kind => Number, Number => Value);

   overriding function "+" (Left : Element; Right : Expression_Type) return Expression_Type is
     (Number (Left) + Right);
   overriding function "+" (Left : Expression_Type; Right : Element) return Expression_Type is
     (Left + Number (Right));

   overriding function "-" (Left : Element; Right : Expression_Type) return Expression_Type is
     (Number (Left) - Right);
   overriding function "-" (Left : Expression_Type; Right : Element) return Expression_Type is
     (Left - Number (Right));

   overriding function "*" (Left : Element; Right : Expression_Type) return Expression_Type is
     (Number (Left) * Right);
   overriding function "*" (Left : Expression_Type; Right : Element) return Expression_Type is
     (Left * Number (Right));

   overriding function "/" (Left : Element; Right : Expression_Type) return Expression_Type is
     (Number (Left) / Right);
   overriding function "/" (Left : Expression_Type; Right : Element) return Expression_Type is
     (Left / Number (Right));

   overriding function Min (Left : Element; Right : Expression_Type) return Expression_Type is
     (Min (Number (Left), Right));
   overriding function Min (Left : Expression_Type; Right : Element) return Expression_Type is
     (Min (Left, Number (Right)));

   overriding function Max (Left : Element; Right : Expression_Type) return Expression_Type is
     (Max (Number (Left), Right));
   overriding function Max (Left : Expression_Type; Right : Element) return Expression_Type is
     (Max (Left, Number (Right)));

   function Generic_Apply
     (Object      : Expression_Type;
      Left, Right : Data_Type) return Data_Type is
   begin
      case Object.Kind is
         when Argument =>
            case Object.Argument is
               when X =>
                  return Left;
               when Y =>
                  return Right;
            end case;
         when Number =>
            return Identity (Object.Number);
         when Binary_Operation =>
            declare
               Result_Left : constant Data_Type :=
                  Generic_Apply (Expression_Type (Object.Left.Element), Left, Right);
               Result_Right : constant Data_Type :=
                  Generic_Apply (Expression_Type (Object.Right.Element), Left, Right);
            begin
               case Object.Operator is
                  when Add =>
                     return Result_Left + Result_Right;
                  when Subtract =>
                     return Result_Left - Result_Right;
                  when Multiply =>
                     return Result_Left * Result_Right;
                  when Divide =>
                     return Result_Left / Result_Right;
                  when Min =>
                     return Min (Result_Left, Result_Right);
                  when Max =>
                     return Max (Result_Left, Result_Right);
               end case;
            end;
         when Unary_Operation =>
            declare
               Result : constant Data_Type :=
                 Generic_Apply (Expression_Type (Object.Expression.Element), Left, Right);
            begin
               case Object.Unary_Operator is
                  when Minus =>
                     return -Result;
                  when Absolute =>
                     return abs Result;
                  when Sqrt =>
                     return Sqrt (Result);
               end case;
            end;
      end case;
   end Generic_Apply;

end Orka.Numerics.Tensors;
