--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2023 onox <denkpadje@gmail.com>
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

with Ada.Characters.Latin_1;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

with GL.Barriers;
with GL.Compute;
with GL.Objects.Buffers;

with Orka.Algorithms.Prefix_Sums;
with Orka.Containers.Bounded_Vectors;
with Orka.Numerics.Tensors.Operations;
with Orka.Rendering.Programs.Shaders;
with Orka.Rendering.Programs.Uniforms;
with Orka.Strings;
with Orka.Types;

package body Orka.Numerics.Tensors.CS_GPU is

   package L1 renames Ada.Characters.Latin_1;
   package SU renames Orka.Strings.SU;

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;
   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   ----------------------------------------------------------------------------
   --                                Constants                               --
   ----------------------------------------------------------------------------

   Max_Element_Wise_Constants : constant := 128;

   Max_Element_Wise_Programs : constant := 256;

   Max_Reduction_Constants : constant := 16;

   Max_Reduction_Programs  : constant := 32;

   ----------------------------------------------------------------------------
   --                              Global state                              --
   ----------------------------------------------------------------------------

   Random_State : Unsigned_32_Array (1 .. 2);

   ----------------------------------------------------------------------------
   --                              Expressions                               --
   ----------------------------------------------------------------------------

   package Expressions is

      type Expression_String is new SU.Unbounded_String;

      Constants : Buffer_Access;

      Offset_Constants : Natural := 0;

      function "+" (Left, Right : Expression_String) return Expression_String is
        ("(" & Left & " + " & Right & ")");

      function "-" (Left, Right : Expression_String) return Expression_String is
        ("(" & Left & " - " & Right & ")");

      function "*" (Left, Right : Expression_String) return Expression_String is
        ("(" & Left & " * " & Right & ")");

      function "/" (Left, Right : Expression_String) return Expression_String is
        ("(" & Left & " / " & Right & ")");

      function Min (Left, Right : Expression_String) return Expression_String is
        ("min(" & Left & ", " & Right & ")");

      function Max (Left, Right : Expression_String) return Expression_String is
        ("max(" & Left & ", " & Right & ")");

      function "-" (Value : Expression_String) return Expression_String is
        ("(-" & Value & ")");

      function "abs" (Value : Expression_String) return Expression_String is
        ("abs(" & Value & ")");

      function Sqrt (Value : Expression_String) return Expression_String is
        ("sqrt(" & Value & ")");

      function Image (Value : Element_Type) return Expression_String;

      function Value (Value : String) return Expression_String is
        (Expression_String (+Value));

      function Image (Value : Expression_String) return String is
        (+SU.Unbounded_String (Value));

   end Expressions;

   package body Expressions is

      function Image (Value : Element_Type) return Expression_String is
      begin
         case Element_Type'Size is
            when 32 =>
               Constants.Set_Data (Float_32_Array'(1 => Float_32 (Value)), Offset_Constants);
            when 64 =>
               Constants.Set_Data (Float_64_Array'(1 => Float_64 (Value)), Offset_Constants);
            when others =>
               raise Constraint_Error with "Element_Type'Size must be 32 or 64";
         end case;

         return Result : constant Expression_String :=
           Expression_String (+("constants[" & Offset_Constants'Image & "]"))
         do
            Offset_Constants := Offset_Constants + 1;
         end return;
      end Image;

   end Expressions;

   use all type Expressions.Expression_String;
   function Apply is new Generic_Apply (Expressions.Expression_String, Expressions.Image);

   function Apply_With_Constants_Buffer
     (Constants   : aliased in out Rendering.Buffers.Buffer;
      Subject     : Expression_Type;
      Left, Right : Expressions.Expression_String) return Expressions.Expression_String is
   begin
      Expressions.Constants := Constants'Unchecked_Access;
      Expressions.Offset_Constants := 0;
      return Result : constant Expressions.Expression_String := Apply (Subject, Left, Right) do
         Expressions.Constants := null;
      end return;
   exception
      when others =>
         Expressions.Constants := null;
         raise;
   end Apply_With_Constants_Buffer;

   ----------------------------------------------------------------------------

   procedure Swap_Rows (Ab : in out GPU_Tensor; I, J : Index_Type) is
   begin
      if I /= J then
         declare
            Row_I : constant GPU_Tensor := Ab (I);
            Old_J : constant GPU_Tensor := Ab (J);
         begin
            Set (Ab, J, Row_I);
            Set (Ab, I, Old_J);
         end;
      end if;
   end Swap_Rows;

   procedure Scale_Row (Ab : in out GPU_Tensor; I : Index_Type; Scale : Element) is
      Row_I : constant GPU_Tensor := Ab (I);
   begin
      if Scale /= 1.0 then
         Set (Ab, I, Scale * Row_I);
      end if;
   end Scale_Row;

   procedure Replace_Row (Ab : in out GPU_Tensor; Scale : Element; I, J : Index_Type) is
      Row_I : constant GPU_Tensor := Ab (I);
      Row_J : constant GPU_Tensor := Ab (J);
   begin
      if Scale /= 0.0 then
         Set (Ab, J, Row_J - Scale * Row_I);
      end if;
   end Replace_Row;

   procedure Forward_Substitute (Ab : in out GPU_Tensor; Index, Pivot_Index : Index_Type) is
      Rows        : constant Natural := Ab.Rows;
      Pivot_Value : constant Element := Ab ([Index, Pivot_Index]);
   begin
      --  Create zeros below the pivot position
      for Row_Index in Index + 1 .. Rows loop
         Replace_Row (Ab, Ab ([Row_Index, Pivot_Index]) / Pivot_Value, Index, Row_Index);
      end loop;
   end Forward_Substitute;

   procedure Back_Substitute (Ab : in out GPU_Tensor; Index, Pivot_Index : Index_Type) is
   begin
      Scale_Row (Ab, Index, 1.0 / Ab ([Index, Pivot_Index]));

      --  Create zeros above the pivot position
      for Row_Index in 1 .. Index - 1 loop
         Replace_Row (Ab, Ab ([Row_Index, Pivot_Index]), Index, Row_Index);
      end loop;
   end Back_Substitute;

   function Create_QR
     (Q, R         : GPU_Tensor;
      Determinancy : Matrix_Determinancy) return GPU_QR_Factorization
   is (Q            => Tensor_Holders.To_Holder (Q),
       R            => Tensor_Holders.To_Holder (R),
       Determinancy => Determinancy);

   procedure Make_Upper_Triangular (Object : in out GPU_Tensor; Offset : Integer := 0) is
      Rows    : constant Natural := Object.Rows;
      Columns : constant Natural := Object.Columns;
   begin
      if Offset >= -(Rows - 2) then
         --  Make matrix upper triangular by zeroing out the elements in the
         --  lower triangular part
         for Row_Index in Index_Type'First + 1 - Integer'Min (1, Offset) .. Rows loop
            for Column_Index in 1 .. Natural'Min (Row_Index - 1 + Offset, Columns) loop
               Object.Set ([Row_Index, Column_Index], 0.0);
            end loop;
         end loop;
         --  TODO Use CS for better performance
      end if;
   end Make_Upper_Triangular;

   package Operations is new Orka.Numerics.Tensors.Operations
     (GPU_Tensor, Make_Upper_Triangular, Scale_Row, Swap_Rows, Forward_Substitute, Back_Substitute,
      Expression_Type, GPU_QR_Factorization, Create_QR, Q, R);

   ----------------------------------------------------------------------------

   function Initialize_Reference return GPU_Tensor_Reference_Access is
     (new GPU_Tensor_Reference'
        (References   => 1,
         Materialized => False,
         Data         => null));

   overriding procedure Adjust (Object : in out GPU_Tensor) is
   begin
      if Object.Reference /= null then
         Object.Reference.References := Object.Reference.References + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out GPU_Tensor) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => GPU_Tensor_Reference, Name => GPU_Tensor_Reference_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Orka.Rendering.Buffers.Buffer, Name => Buffer_Access);
   begin
      if Object.Reference /= null then
         Object.Reference.References := Object.Reference.References - 1;
         if Object.Reference.References = 0 then
            if Object.Reference.Data /= null then
               Free (Object.Reference.Data);
            end if;
            Free (Object.Reference);
         end if;
      end if;

      --  Idempotence: next call to Finalize has no effect
      Object.Reference := null;
   end Finalize;

   ----------------------------------------------------------------------------

   function Largest_Group_Size (Maximum_Size : Size) return Positive is
      Result : Size := 1;
   begin
      while Result * 2 <= Maximum_Size loop
         Result := Result * 2;
      end loop;

      return Positive (Result);
   end Largest_Group_Size;

   Element_In_Bytes : constant Positive := Element'Size / System.Storage_Unit;

   function Maximum_Buffer_Size return Integer_32
     renames GL.Objects.Buffers.Max_Shader_Storage_Block_Size;

   procedure Create_Buffer (Object : GPU_Tensor)
     with Pre => Object.Reference.Data = null
                   and Object.Elements > 0
                   and Object.Elements <= Positive (Maximum_Buffer_Size) / Element_In_Bytes;
   --  See https://github.com/KhronosGroup/OpenGL-API/issues/36
   --  TODO Add function Limits/Capabilities to type Context in Orka.Contexts

   function From_Kind (Kind : Data_Type) return Orka.Types.Element_Type is
     (case Kind is
        when Int_Type   => Orka.Types.Int_Type,
        when Bool_Type  => Orka.Types.UInt_Type,
        when Float_Type =>
          (case Element'Size is
             when 32     => Orka.Types.Single_Type,
             when 64     => Orka.Types.Double_Type,
             when others => raise Constraint_Error with "Element_Type'Size must be 32 or 64"));

   procedure Create_Buffer (Object : GPU_Tensor) is
      use Orka.Rendering.Buffers;
   begin
      Object.Reference.Data := new Buffer'(Create_Buffer
        (Flags  => (Dynamic_Storage => True, others => False),
         Kind   => From_Kind (Object.Kind),
         Length => Object.Elements));
   end Create_Buffer;

   ----------------------------------------------------------------------------

   type Kernel_Type (Materialized : Boolean) is record
      case Materialized is
         when True  => Buffer : not null Buffer_Access;
         when False => Text   : SU.Unbounded_String;
      end case;
   end record;

   package Buffer_Vectors is new Orka.Containers.Bounded_Vectors (Natural, Buffer_Access);

   function Materialize_Tensor
     (Object : in out GPU_Tensor) return not null Buffer_Access;

   function Build_Kernel
     (Object           : in out GPU_Tensor;
      Buffers          : in out Buffer_Vectors.Vector;
      Variable         : in out Positive;
      Constants        : aliased Rendering.Buffers.Buffer;
      Offset_Constants : in out Natural;
      Needs_Shape      : in out Boolean) return Kernel_Type;

   function Name (Variable : Positive) return String is
      Name : constant String := Variable'Image;
   begin
      return "v" & Name (Name'First + 1 .. Name'Last);
   end Name;

   function Name_Of_Buffer (Index : Natural) return String is
      Name : constant String := Index'Image;
   begin
      return "buffer" & Name (Name'First + 1 .. Name'Last);
   end Name_Of_Buffer;

   function Name_Buffer
     (Buffers : in out Buffer_Vectors.Vector;
      Buffer  : Buffer_Access) return String
   is
      Index_Buffer : Natural;
      Found_Buffer : Boolean := False;

      procedure Find_Index (Elements : Buffer_Vectors.Element_Array) is
      begin
         for Index in Elements'Range loop
            if Elements (Index) = Buffer then
               Index_Buffer := Index;
               Found_Buffer := True;
               exit;
            end if;
         end loop;
      end Find_Index;
   begin
      Buffers.Query (Find_Index'Access);

      if not Found_Buffer then
         Index_Buffer := Buffers.Length;
         Buffers.Append (Buffer);
      end if;

      return Name_Of_Buffer (Index_Buffer);
   end Name_Buffer;

   function Data_Type_Image (Kind : Data_Type) return String is
     (case Kind is
        when Int_Type   => "int",
        when Bool_Type  => "bool",
        when Float_Type =>
          (case Element'Size is
             when 32     => "float",
             when 64     => "double",
             when others => raise Constraint_Error with "Element_Type'Size must be 32 or 64"));

   function Value_Zero (Kind : Data_Type) return String is
     (case Kind is
        when Int_Type   => "0",
        when Bool_Type  => "false",
        when Float_Type => "0.0");

   function Data_Type_Repr (Kind : Data_Type) return Unsigned_32 is
     (case Kind is
       when Int_Type   => 0,
       when Bool_Type  => 1,
       when Float_Type =>
         (case Element'Size is
            when 32     => 2,
            when 64     => 3,
            when others => raise Constraint_Error with "Element_Type'Size must be 32 or 64"));

   function Get_Shader
     (Object           : in out GPU_Tensor;
      Buffers          : in out Buffer_Vectors.Vector;
      Variable         : in out Positive;
      Constants        : aliased Rendering.Buffers.Buffer;
      Offset_Constants : in out Natural;
      Needs_Shape      : in out Boolean) return SU.Unbounded_String
   is
      Type_String : constant String := Data_Type_Image (Object.Kind);

      Current_Variable : constant Positive := Variable;

      function Assign (Text : String) return SU.Unbounded_String is
        (+(Name (Current_Variable) & " = " & Text & ";"));

      function Initialize (Text : String) return SU.Unbounded_String is
        (+(Type_String & " " & Name (Current_Variable) & " = " & Text & ";"));

      function To_Binary_Function (Name, Left, Right : String) return String is
        (if Name = "pow" then
           "float(" & Left & ") == 0.0 && float(" & Right & ") == 0.0" &
            " ? 1.0 : pow(float(" & Left & "), float(" & Right & "))"
         else
           Name & "(" & Left & ", " & Right & ")");

      function To_Binary_Operator (Operator, Left, Right : String) return String is
        (if Operator = "&" then
           Right & " ? " & Left & " : " & Value_Zero (Object.Kind)
         else
           Left & " " & Operator & " " & Right);
      --  Function "and" supports Float_Type and Bool_Type for Left parameter

      function To_Unary_Function (Name, Value : String) return String is
        (if Element'Size = 64 and
           Name in "exp" | "log" | "log2" | "sin" | "cos" | "tan" | "asin" | "acos"
         then
           "double(" & Name & "(float(" & Value & ")))"
         else
           Name & "(" & Value & ")");
      --  Some functions are available only for 32-bit floats in GLSL

      Result : SU.Unbounded_String;

      procedure Prepend_Variable_Text (Expression : in out Tensor'Class) is
         Kernel : constant Kernel_Type :=
           Build_Kernel (GPU_Tensor (Expression), Buffers, Variable, Constants, Offset_Constants, Needs_Shape);
      begin
         case Kernel.Materialized is
            when True  =>
               SU.Append (Result,
                 Data_Type_Image (Expression.Kind) & " " & Name (Variable) & " = " &
                   Name_Buffer (Buffers, Kernel.Buffer) & "[gid];");
               SU.Append (Result, L1.LF);
            when False =>
               SU.Append (Result, Kernel.Text);
         end case;
      end Prepend_Variable_Text;

      function Get_Name (Increment : Boolean; Expression : in out Tensor'Class) return String is
      begin
         if Increment then
            Variable := Variable + 1;
         end if;

         declare
            Result : constant String := Name (Variable);
         begin
            Prepend_Variable_Text (Expression);
            return Result;
         end;
      end Get_Name;

      function Get_Constant (Value : Element) return String is
      begin
         case Element_Type'Size is
            when 32 =>
               Constants.Set_Data (Float_32_Array'(1 => Float_32 (Value)), Offset_Constants);
            when 64 =>
               Constants.Set_Data (Float_64_Array'(1 => Float_64 (Value)), Offset_Constants);
            when others =>
               raise Constraint_Error with "Element_Type'Size must be 32 or 64";
         end case;

         return Result : constant String := "constants[" & Offset_Constants'Image & "]" do
            Offset_Constants := Offset_Constants + 1;
         end return;
      end Get_Constant;
   begin
      case Object.Operation.Kind is
         when Binary_Operation =>
            declare
               Is_Boolean_Result : constant Boolean :=
                 Object.Operation.Binary_Operator in Boolean_Operator
                   and not
                     (Object.Operation.Binary_Operator = Logical_And and Object.Kind /= Bool_Type);
               pragma Assert (if Is_Boolean_Result then Object.Kind = Bool_Type);

               Left : constant String :=
                 (case Object.Operation.Left.Kind is
                    when Tensor_Value =>
                      Get_Name (Is_Boolean_Result, Object.Operation.Left.Tensor.Reference),
                    when Scalar_Value => Get_Constant (Object.Operation.Left.Value));

               Right : constant String :=
                 (case Object.Operation.Right.Kind is
                    when Tensor_Value => Get_Name (True, Object.Operation.Right.Tensor.Reference),
                    when Scalar_Value => Get_Constant (Object.Operation.Right.Value));

               procedure Assign_Or_Initialize (Value : String) is
                  Reuse : constant Boolean := Object.Operation.Left.Kind = Tensor_Value
                    and not Is_Boolean_Result;
               begin
                  SU.Append (Result, (if Reuse then Assign (Value) else Initialize (Value)));
               end Assign_Or_Initialize;
            begin
               if Object.Operation.Binary_Operator in Binary_Operator then
                  Assign_Or_Initialize (To_Binary_Operator
                    ((case Object.Operation.Binary_Operator is
                       when Add           => "+",
                       when Subtract      => "-",
                       when Multiply      => "*",
                       when Divide        => "/",
                       when Equal         => "==",
                       when Not_Equal     => "!=",
                       when Greater_Than  => ">",
                       when Greater_Equal => ">=",
                       when Less_Than     => "<",
                       when Less_Equal    => "<=",
                       when Logical_And   => (if Object.Kind /= Bool_Type then "&" else "&&"),
                       when Logical_Or    => "||",
                       when Logical_Xor   => "^^",
                       when others        => raise Program_Error), Left, Right));
               elsif Object.Operation.Binary_Operator in Binary_Function then
                  Assign_Or_Initialize (To_Binary_Function
                    ((case Object.Operation.Binary_Operator is
                       when Power   => "pow",  --  Undefined if (x = 0 and y <= 0) or x < 0
                       when Modulus => "mod",
                       when Min     => "min",
                       when Max     => "max",
                       when Arctan  => "atan",
                       when others  => raise Program_Error), Left, Right));
               elsif Object.Operation.Binary_Operator = Divide_Or_Zero then
                  Assign_Or_Initialize (Right & " != 0 ? " & Left & " / " & Right & " : 0.0");
               else
                  raise Program_Error;
               end if;
            end;
         when Unary_Operation =>
            declare
               Value : constant String :=
                 (case Object.Operation.Value.Kind is
                    when Tensor_Value => Get_Name (False, Object.Operation.Value.Tensor.Reference),
                    when Scalar_Value => Get_Constant (Object.Operation.Value.Value));
            begin
               SU.Append (Result, Assign (To_Unary_Function
                 ((case Object.Operation.Unary_Operator is
                    when Minus         => "-",
                    when Absolute      => "abs",
                    when Sqrt          => "sqrt",  --  Undefined if x < 0
                    when Ceil          => "ceil",
                    when Floor         => "floor",
                    when Round         => "round",
                    when Truncate      => "trunc",
                    when Exp           => "exp",   --                       (No Double in GLSL)
                    when Log           => "log",   --  Undefined if x <= 0  (No Double in GLSL)
                    when Log2          => "log2",  --  Undefined if x <= 0  (No Double in GLSL)
                    when Sin           => "sin",   --                       (No Double in GLSL)
                    when Cos           => "cos",   --                       (No Double in GLSL)
                    when Tan           => "tan",   --                       (No Double in GLSL)
                    when Arcsin        => "asin",  --  Undefined if |x| > 1 (No Double in GLSL)
                    when Arccos        => "acos",  --  Undefined if |x| > 1 (No Double in GLSL)
                    when Logical_Not   => "!",
                    when Reshape       => ""), Value)));
            end;
         when Constructor_Operation =>
            case Object.Operation.Constructor.Kind is
               when Identity =>
                  declare
                     Offset : constant Integer := Object.Operation.Constructor.Offset;
                  begin
                     SU.Append (Result, "const uint row = uint(floor(gid / shape.y));" & L1.LF);
                     SU.Append (Result, "const uint column = uint(mod(gid, shape.y));" & L1.LF);
                     SU.Append (Result,
                       "const uint index = " & Offset'Image & " + row * (shape.y + 1);" & L1.LF);
                     SU.Append (Result, Initialize (Type_String & "(index == gid)"));

                     Needs_Shape := True;
                  end;
               when Fill =>
                  SU.Append (Result, Initialize
                    (Get_Constant (Object.Operation.Constructor.Value)));
               when Linear_Space =>
                  pragma Assert (Object.Kind = Float_Type);

                  declare
                     Start : constant String := Get_Constant (Object.Operation.Constructor.Start);
                     Step  : constant String := Get_Constant (Object.Operation.Constructor.Step);
                  begin
                     SU.Append (Result, Initialize (Start & " + " & Step & " * gid"));
                  end;
               when Log_Space =>
                  pragma Assert (Object.Kind = Float_Type);

                  declare
                     Start : constant String := Get_Constant (Object.Operation.Constructor.Start);
                     Step  : constant String := Get_Constant (Object.Operation.Constructor.Step);
                     Base  : constant String := Get_Constant (Object.Operation.Constructor.Base);
                  begin
                     SU.Append (Result, Initialize
                       ("pow(float(" & Base & "), float(" & Start & " + " & Step & " * gid))"));
                  end;
            end case;
         when Matrix_Operation =>
            raise Program_Error;
         when None =>
            --  If tensor has no operations to perform, then it should already have
            --  been materialized and thus there would be no need to call Get_Shader
            raise Program_Error;
      end case;

      SU.Append (Result, L1.LF);
      return Result;
   end Get_Shader;

   procedure Materialize_Tensor (Object : GPU_Tensor)
     with Pre  => Object.Elements > 0,
          Post => Object.Is_Materialized;

   function Materialize_Tensor
     (Object : in out GPU_Tensor) return not null Buffer_Access is
   begin
      Object.Materialize_Tensor;
      return Object.Reference.Data;
   end Materialize_Tensor;

   function Build_Kernel
     (Object           : in out GPU_Tensor;
      Buffers          : in out Buffer_Vectors.Vector;
      Variable         : in out Positive;
      Constants        : aliased Rendering.Buffers.Buffer;
      Offset_Constants : in out Natural;
      Needs_Shape      : in out Boolean) return Kernel_Type is
   begin
      if Object.Reference.Materialized then
         return (Materialized => True, Buffer => Object.Reference.Data);
      elsif Object.Reference.References > 2 or Object.Operation.Kind = Matrix_Operation then
         return (Materialized => True, Buffer => Materialize_Tensor (Object));
      else
         return
           (Materialized => False,
            Text         => Get_Shader (Object, Buffers, Variable, Constants, Offset_Constants, Needs_Shape));
      end if;
   end Build_Kernel;

   ----------------------------------------------------------------------------

   type Program_Array is array (Data_Type) of Rendering.Programs.Shader_Program;

   type Reduction_Program is record
      Size    : Positive;
      Hash    : Ada.Containers.Hash_Type;
      Program : Rendering.Programs.Shader_Program;
   end record;

   type Element_Wise_Program is record
      Hash    : Ada.Containers.Hash_Type;
      Program : Rendering.Programs.Shader_Program;
   end record;

   package Reduction_Program_Vectors is new Orka.Containers.Bounded_Vectors
     (Natural, Reduction_Program);

   package Element_Wise_Program_Vectors is new Orka.Containers.Bounded_Vectors
     (Natural, Element_Wise_Program);

   type Kernel_Programs is tagged record
      Source_Element_Wise : SU.Unbounded_String;
      Source_Reduce_Assoc : SU.Unbounded_String;
      Source_Reduce       : SU.Unbounded_String;

      Reduction_Programs    : Reduction_Program_Vectors.Vector (Max_Reduction_Programs);
      Element_Wise_Programs : Element_Wise_Program_Vectors.Vector (Max_Element_Wise_Programs);

      Program_Main_Diagonal  : Program_Array;
      Program_Diagonal       : Program_Array;
      Program_Transpose      : Program_Array;
      Program_Matrix_Matrix  : Program_Array;
      Program_Random         : Program_Array;
      Program_Compact_Tensor : Program_Array;
      Program_Is_True        : Rendering.Programs.Shader_Program;
   end record;

   type Kernel_Programs_Access is access Kernel_Programs;

   Kernels : Kernel_Programs_Access;
   Location_Prefix_Sum, Location_Tensors_GPU  : Resources.Locations.Location_Access;
   Context : Orka.Contexts.Context_Access;

   procedure Initialize_Shaders
     (Context                 : Orka.Contexts.Context_Access;
      Prefix_Sum, Tensors_GPU : Resources.Locations.Location_Ptr) is
   begin
      Location_Prefix_Sum  := Prefix_Sum;
      Location_Tensors_GPU := Tensors_GPU;
      CS_GPU.Context       := Context;
   end Initialize_Shaders;

   function Create_Kernels return not null Kernel_Programs_Access is
      use Rendering.Programs;

      function Get_Shader (Path : String) return String is
         Source : constant Resources.Byte_Array_Pointers.Pointer :=
           Location_Tensors_GPU.Read_Data (Path);
      begin
         return Resources.Convert (Source.Get);
      end Get_Shader;

      function Get_Kernel (Kind : Data_Type; Text : SU.Unbounded_String) return Shader_Program is
         Source : SU.Unbounded_String := Text;
      begin
         Strings.Replace (Source, "%DATA_TYPE%", Data_Type_Image (Kind));
         Strings.Replace (Source, "%DATA_TYPE_REPR%", Data_Type_Repr (Kind)'Image);
         Strings.Replace (Source, "%VALUE_ZERO%", Value_Zero (Kind));

         return Create_Program_From_Source (Compute_Shader, +Source);
      end Get_Kernel;

      function Get_Kernel (Text : SU.Unbounded_String) return Program_Array is
        [Int_Type   => Get_Kernel (Int_Type, Text),
         Bool_Type  => Get_Kernel (Int_Type, Text),
         Float_Type => Get_Kernel (Float_Type, Text)];

      Shader_Text_Main_Diagonal  : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/main-diagonal.comp");
      Shader_Text_Diagonal       : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/diagonal.comp");
      Shader_Text_Transpose      : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/transpose.comp");
      Shader_Text_Matrix_Matrix  : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/matrix-multiplication.comp");
      Shader_Text_Random         : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/xoshiro.comp");
      Shader_Text_Compact_Tensor : constant SU.Unbounded_String :=
        +Get_Shader ("tensors/compact-tensor.comp");
   begin
      return
        new Kernel_Programs'
          (Source_Element_Wise => +Get_Shader ("tensors/element-wise.comp"),
           Source_Reduce_Assoc => +Get_Shader ("tensors/reduce-associative.comp"),
           Source_Reduce       => +Get_Shader ("tensors/reduce.comp"),

           Program_Main_Diagonal  => Get_Kernel (Shader_Text_Main_Diagonal),
           Program_Diagonal       => Get_Kernel (Shader_Text_Diagonal),
           Program_Transpose      => Get_Kernel (Shader_Text_Transpose),
           Program_Matrix_Matrix  => Get_Kernel (Shader_Text_Matrix_Matrix),
           Program_Random         => Get_Kernel (Shader_Text_Random),
           Program_Compact_Tensor => Get_Kernel (Shader_Text_Compact_Tensor),

           Program_Is_True => Create_Program_From_Source (Compute_Shader, Get_Shader ("tensors/is-true.comp")),

           Reduction_Programs    => <>,
           Element_Wise_Programs => <>);
   end Create_Kernels;

   ----------------------------------------------------------------------------

   function Groups (Elements, Group_Size : Positive) return Unsigned_32 is
     (Unsigned_32 (Elements / Group_Size + (if Elements mod Group_Size = 0 then 0 else 1)));

   procedure Materialize_Tensor (Object : GPU_Tensor) is
   begin
      if Object.Reference.Materialized then
         return;
      end if;

      if Object.Reference.Data = null then
         Create_Buffer (Object);
      end if;

      if Kernels = null then
         Kernels := Create_Kernels;
      end if;

      declare
         use Rendering.Buffers;
         use Rendering.Programs;
         use all type Rendering.Buffers.Indexed_Buffer_Target;
         use type SU.Unbounded_String;

         Copy : GPU_Tensor := Object;

         Buffers : Buffer_Vectors.Vector
           (Capacity => Positive (GL.Objects.Buffers.Max_Compute_Shader_Storage_Blocks));

         procedure Bind_Buffers (Elements : Buffer_Vectors.Element_Array) is
         begin
            for Index in Elements'Range loop
               Elements (Index).Bind (Shader_Storage, Index);
            end loop;
         end Bind_Buffers;

         function Get_Kernel_Element_Wise
           (Programs  : in out Element_Wise_Program_Vectors.Vector;
            Constants : aliased Buffer;
            Needs_Shape : in out Boolean) return Shader_Program
         is
            Variable : Positive := 1;

            Offset_Constants : Natural := 0;

            Text : constant SU.Unbounded_String :=
              Get_Shader (Copy, Buffers, Variable, Constants, Offset_Constants, Needs_Shape) &
              Name_Buffer (Buffers, Object.Reference.Data) & "[gid] = " & Name (1) & ";";

            Source : SU.Unbounded_String := Kernels.Source_Element_Wise;

            Buffers_Source : SU.Unbounded_String;

            procedure Append_Definition
              (Name  : String;
               Kind  : Orka.Types.Element_Type;
               Index : Natural)
            is
               use all type Orka.Types.Element_Type;

               Buffer_Type : constant String :=
                 (case Kind is
                    when Int_Type    => "int",
                    when UInt_Type   => "bool",
                    when Single_Type => "float",
                    when Double_Type => "double",
                    when others => raise Program_Error);
            begin
               SU.Append (Buffers_Source,
                 "layout(std430, binding = " & Index'Image & ") restrict buffer " &
                 "data_" & Name & " { " & Buffer_Type & " " & Name & "[]; };" & L1.LF);
            end Append_Definition;

            procedure Append_Shader_Text_Buffers (Elements : Buffer_Vectors.Element_Array) is
            begin
               for Index in Elements'Range loop
                  declare
                     Name : constant String := Name_Of_Buffer (Index);
                  begin
                     Append_Definition (Name, Elements (Index).Kind, Index);
                  end;
               end loop;
            end Append_Shader_Text_Buffers;
         begin
            Buffers.Query (Append_Shader_Text_Buffers'Access);
            Append_Definition ("constants", Constants.Kind, Buffers.Length);

            Strings.Replace (Source, "%BUFFERS%", +Buffers_Source);
            Strings.Replace (Source, "%OPERATIONS%", +Text);

            declare
               Hash : constant Ada.Containers.Hash_Type := Ada.Strings.Hash (+Source);

               Index_Program : Natural;
               Found_Program : Boolean := False;

               procedure Find_Index (Elements : Element_Wise_Program_Vectors.Element_Array) is
                  use type Ada.Containers.Hash_Type;
               begin
                  for Index in Elements'Range loop
                     if Elements (Index).Hash = Hash then
                        Index_Program := Index;
                        Found_Program := True;
                        exit;
                     end if;
                  end loop;
               end Find_Index;
            begin
               Programs.Query (Find_Index'Access);

               if Found_Program then
                  return Programs (Index_Program).Program;
               end if;

               return Result : constant Shader_Program := Create_Program_From_Source (Compute_Shader, +Source) do
                  Programs.Append ((Hash, Result));
               end return;
            end;
         end Get_Kernel_Element_Wise;

         procedure Set_Shape (Kernel : Shader_Program; Shape : Tensor_Shape) is
            Shape_Vector : Unsigned_32_Array (1 .. 4) := [others => 0];
         begin
            for Index in Shape'Range loop
               Shape_Vector (Size (Index)) := Unsigned_32 (Shape (Index));
            end loop;

            Kernel.Uniform ("shape").Set_Vector (Shape_Vector);
         exception
            when Rendering.Programs.Uniforms.Uniform_Inactive_Error =>
               null;
         end Set_Shape;

         procedure Set_Count (Kernel : Shader_Program; Count : Natural) is
         begin
            Kernel.Uniform ("count").Set_UInt (Unsigned_32 (Count));
         exception
            when Rendering.Programs.Uniforms.Uniform_Inactive_Error =>
               null;
         end Set_Count;

         procedure Initialize_Element_Wise
           (Kernel    : Shader_Program;
            Constants : aliased in out Buffer;
            Needs_Shape : Boolean)
         is
            Elements : constant Positive := Object.Elements;
         begin
            --  Uniform 'shape' is needed only when the identity constructor
            --  is used, otherwise it is optimized out by the GLSL compiler
            if Needs_Shape then
               Set_Shape (Kernel, Object.Shape);
            end if;
            Set_Count (Kernel, Elements);

            pragma Assert (for all Buffer of Buffers => Buffer.all.Length = Elements);

            Buffers.Append (Constants'Unchecked_Access);
         end Initialize_Element_Wise;

         procedure Initialize_Main_Diagonal (Kernel : Shader_Program) is
            Source : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Value.Reference));
         begin
            Buffers.Append (Materialize_Tensor (Source));
            Buffers.Append (Object.Reference.Data);

            Set_Shape (Kernel, Source.Shape);
            Kernel.Uniform ("offset").Set_Integer (Object.Operation.Matrix_Operation.Offset);

            pragma Assert (Source.Axes = 2);
            pragma Assert (Object.Axes = 1);
         end Initialize_Main_Diagonal;

         procedure Initialize_Diagonal (Kernel : Shader_Program) is
            Source : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Value.Reference));
         begin
            Buffers.Append (Materialize_Tensor (Source));
            Buffers.Append (Object.Reference.Data);

            Set_Shape (Kernel, Object.Shape);
            Kernel.Uniform ("offset").Set_Integer (Object.Operation.Matrix_Operation.Offset);

            pragma Assert (Source.Axes = 1);
            pragma Assert (Object.Axes = 2);
         end Initialize_Diagonal;

         procedure Initialize_Transpose (Kernel : Shader_Program) is
            Source : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Value.Reference));
         begin
            Buffers.Append (Materialize_Tensor (Source));
            Buffers.Append (Object.Reference.Data);

            Set_Shape (Kernel, Object.Shape);
            Set_Count (Kernel, Object.Elements);

            pragma Assert (Source.Axes = 2);
            pragma Assert (Object.Axes = 2);
         end Initialize_Transpose;

         procedure Initialize_Matrix_Matrix (Kernel : Shader_Program) is
            Source_Left : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Left.Reference));
            Source_Right : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Right.Reference));
         begin
            Buffers.Append (Materialize_Tensor (Source_Left));
            Buffers.Append (Materialize_Tensor (Source_Right));
            Buffers.Append (Object.Reference.Data);

            case Object.Axes is
               when 1 => Set_Shape (Kernel, [1 => Object.Rows, 2 => 1]);
               when 2 => Set_Shape (Kernel, Object.Shape);
               when others => raise Not_Implemented_Yet;  --  FIXME
            end case;
            Kernel.Uniform ("size").Set_UInt (Unsigned_32 (Source_Left.Columns));
            pragma Assert (Source_Left.Columns = Source_Right.Rows);
            pragma Assert (Source_Right.Axes = Object.Axes);

            --  TODO Shouldn't Source_Left also be able to be a row vector, e.g. Axes <= 2?
            pragma Assert (Source_Left.Axes = 2);
            pragma Assert (Source_Right.Axes <= 2);
            pragma Assert (Object.Axes <= 2);
         end Initialize_Matrix_Matrix;

         procedure Initialize_Random (Kernel : Shader_Program) is
         begin
            Buffers.Append (Object.Reference.Data);

            --  Actual seed in shader uses Random_State and the clock from
            --  the ARB_shader_clock extension
            Kernel.Uniform ("seed").Set_Vector (Random_State);
         end Initialize_Random;

         procedure Initialize_Is_True (Kernel : Shader_Program; Is_All : Boolean) is
            Source : GPU_Tensor :=
              GPU_Tensor (Tensor'Class'(Copy.Operation.Matrix_Operation.Value.Reference));
         begin
            Buffers.Append (Materialize_Tensor (Source));
            Buffers.Append (Object.Reference.Data);

            Kernel.Uniform ("is_all").Set_Boolean (Is_All);

            pragma Assert (Source.Kind = Bool_Type);
            pragma Assert (Object.Kind = Bool_Type);
            pragma Assert (Object.Axes = 1);
         end Initialize_Is_True;

         Buffer_Constants : aliased Buffer := Create_Buffer
           (Flags  => (Dynamic_Storage => True, others => False),
            Kind   => (case Element_Type'Size is
                         when 32 => Orka.Types.Single_Type,
                         when 64 => Orka.Types.Double_Type,
                         when others =>
                            raise Constraint_Error with "Element_Type'Size must be 32 or 64"),
            Length => (if Object.Operation.Kind = Matrix_Operation then
                         1
                       else
                         Max_Element_Wise_Constants));

         Needs_Shape : Boolean := False;

         Kernel : constant Shader_Program :=
           (if Object.Operation.Kind = Matrix_Operation then
              (case Object.Operation.Matrix_Operation.Kind is
                 when Main_Diagonal       => Kernels.Program_Main_Diagonal (Object.Kind),
                 when Diagonal            => Kernels.Program_Diagonal (Object.Kind),
                 when Transpose           => Kernels.Program_Transpose (Object.Kind),
                 when Matrix_Matrix       => Kernels.Program_Matrix_Matrix (Object.Kind),
                 when Random              => Kernels.Program_Random (Object.Kind),
                 when Any_True | All_True => Kernels.Program_Is_True)
            else
              Get_Kernel_Element_Wise (Kernels.Element_Wise_Programs, Buffer_Constants, Needs_Shape));

         Work_Group_Size : constant Dimension_Size_Array := Kernel.Compute_Work_Group_Size;

         Size_X   : constant Positive := Positive (Work_Group_Size (X));
         Size_Y   : constant Positive := Positive (Work_Group_Size (Y));

         Elements : constant Positive :=
           (if Object.Operation.Kind = Matrix_Operation
                 and then Object.Operation.Matrix_Operation.Kind in Any_True | All_True
            then
               Object.Operation.Matrix_Operation.Value.Constant_Reference.Elements
            else
               Object.Elements);

         use Rendering.Programs.Shaders;
      begin
         if Object.Operation.Kind = Matrix_Operation then
            case Object.Operation.Matrix_Operation.Kind is
               when Main_Diagonal => Initialize_Main_Diagonal (Kernel);
               when Diagonal      => Initialize_Diagonal (Kernel);
               when Transpose     => Initialize_Transpose (Kernel);
               when Matrix_Matrix => Initialize_Matrix_Matrix (Kernel);
               when Random        => Initialize_Random (Kernel);
               when Any_True      => Initialize_Is_True (Kernel, False);
               when All_True      => Initialize_Is_True (Kernel, True);
            end case;
         else
            Initialize_Element_Wise (Kernel, Buffer_Constants, Needs_Shape);
         end if;

         Context.Bind_Shaders ((Compute_Shader => From (Kernel), others => Empty));

         Buffers.Query (Bind_Buffers'Access);

         GL.Barriers.Memory_Barrier
           ((Shader_Storage => True, others => False));

         if Object.Operation.Kind = Matrix_Operation
           and then Object.Operation.Matrix_Operation.Kind in Diagonal | Matrix_Matrix
           and then Object.Axes = 2
         then
            GL.Compute.Dispatch_Compute
              (X => Groups (Elements => Object.Rows,    Group_Size => Size_X),
               Y => Groups (Elements => Object.Columns, Group_Size => Size_Y));
         elsif Object.Operation.Kind = Matrix_Operation
           and then Object.Operation.Matrix_Operation.Kind = Random
         then
            GL.Compute.Dispatch_Compute (X => 1);
         else
            GL.Compute.Dispatch_Compute
              (X => Groups (Elements => Elements, Group_Size => Size_X));
         end if;
      end;

      GL.Barriers.Memory_Barrier
        ((Buffer_Update | Shader_Storage => True, others => False));

      Object.Reference.Materialized := True;
   end Materialize_Tensor;

   ----------------------------------------------------------------------------

   function Without_Data
     (Object : GPU_Tensor;
      Kind   : Data_Type := Float_Type) return GPU_Tensor
   with Post => Without_Data'Result.Reference /= null;
   --  This silly definition is needed to avoid "length check failed" in GNAT FSF 11.1

   function Without_Data
     (Shape : Tensor_Shape;
      Kind  : Data_Type := Float_Type) return GPU_Tensor
   is
     ((Ada.Finalization.Controlled with
         Axes      => Shape'Length,
         Kind      => Kind,
         Operation => (Kind => None),
         Reference => Initialize_Reference,
         Shape     => Shape));

   function Without_Data
     (Object : GPU_Tensor;
      Kind   : Data_Type := Float_Type) return GPU_Tensor
   is (Without_Data (Object.Shape, Kind));

   function With_Buffer
     (Shape : Tensor_Shape;
      Kind  : Data_Type := Float_Type) return GPU_Tensor is
   begin
      return Result : constant GPU_Tensor := Without_Data (Shape, Kind) do
         Create_Buffer (Result);
         Result.Reference.Materialized := True;
      end return;
   end With_Buffer;

   function From_Constructor
     (Shape       : Tensor_Shape;
      Kind        : Data_Type := Float_Type;
      Constructor : Constructor_Type) return GPU_Tensor
   is
     ((Ada.Finalization.Controlled with
         Axes      => Shape'Length,
         Kind      => Kind,
         Reference => Initialize_Reference,
         Shape     => Shape,
         Operation => (Kind => Constructor_Operation, Constructor => Constructor)));

   function From_Matrix_Operation
     (Shape     : Tensor_Shape;
      Kind      : Data_Type := Float_Type;
      Operation : Matrix_Operation_Type) return GPU_Tensor
   is
     ((Ada.Finalization.Controlled with
         Axes      => Shape'Length,
         Kind      => Kind,
         Reference => Initialize_Reference,
         Shape     => Shape,
         Operation => (Kind => Matrix_Operation, Matrix_Operation => Operation)));

   function From_Unary_Operation
     (Shape    : Tensor_Shape;
      Kind     : Data_Type := Float_Type;
      Operator : Unary_Operation_Kind;
      Value    : Value_Type) return GPU_Tensor
   is
     ((Ada.Finalization.Controlled with
         Axes       => Shape'Length,
         Kind       => Kind,
         Reference  => Initialize_Reference,
         Shape      => Shape,
         Operation  => (Kind           => Unary_Operation,
                        Unary_Operator => Operator,
                        Value          => Value)));

   function From_Binary_Operation
     (Shape       : Tensor_Shape;
      Kind        : Data_Type := Float_Type;
      Operator    : Binary_Operation_Kind;
      Left, Right : Value_Type) return GPU_Tensor
   is
     ((Ada.Finalization.Controlled with
         Axes       => Shape'Length,
         Kind       => Kind,
         Reference  => Initialize_Reference,
         Shape      => Shape,
         Operation  => (Kind            => Binary_Operation,
                        Binary_Operator => Operator,
                        Left            => Left,
                        Right           => Right)));

   ----------------------------------------------------------------------------

   overriding procedure Materialize (Object : in out GPU_Tensor) is
   begin
      Object.Materialize_Tensor;
   end Materialize;

   overriding function Is_Materialized (Object : GPU_Tensor) return Boolean is
     (Object.Reference.Materialized and Object.Reference.Data /= null);

   overriding function Kind (Object : GPU_Tensor) return Data_Type is (Object.Kind);

   overriding
   function Get (Object : GPU_Tensor; Index : Index_Type) return Element renames Operations.Get;

   overriding
   function Get (Object : GPU_Tensor; Index : Index_Type) return Boolean renames Operations.Get;

   overriding function Get (Object : GPU_Tensor; Index : Index_Type) return GPU_Tensor is
      Count : constant Positive := Object.Columns;
      Shape : constant Tensor_Shape := [1 => Count];
   begin
      if Index > Object.Rows then
         raise Constraint_Error with
           "Stop index (" & Trim (Index) & ") out of bounds (1 .. " & Trim (Object.Rows) & ")";
      end if;

      Object.Materialize_Tensor;

      --  Returning the row of a 2D tensor as a vector instead of a (1, n) 2D tensor
      return Result : constant GPU_Tensor := With_Buffer (Shape, Object.Kind) do
         Object.Reference.Data.Copy_Data
           (Result.Reference.Data.all, (Index - 1) * Count, 0, Count);
      end return;
   end Get;

   overriding procedure Set
     (Object : in out GPU_Tensor;
      Index  : Index_Type;
      Value  : GPU_Tensor) renames Operations.Set;

   overriding procedure Set
     (Object : in out GPU_Tensor;
      Index  : Range_Type;
      Value  : GPU_Tensor) renames Operations.Set;

   overriding
   procedure Set (Object : in out GPU_Tensor; Index : Tensor_Range; Value : GPU_Tensor) is
      Full_Index : constant Tensor_Range := Full_Range (Object.Shape, Index);
      Full_Value : constant Tensor_Shape := Full_Shape (Object.Axes, Value.Shape, Right);

      pragma Assert (Full_Value = Shape (Full_Index));
   begin
      --  If the value (and shape of index) has the full depth/height/width except
      --  for the first axis, then the memory to which the data will be written
      --  is contiguous, which means it has no gaps.
      --
      --  For example, if shape of Value is (2, 3) and you have the following
      --  object and index (in brackets):
      --
      --  1 [ 2  3  4]  5
      --  6 [ 7  8  9] 10
      --  11 12 13 14  15
      --
      --  then there is a gap (positions 5 and 6). Howerver, if the shape
      --  of Value is (2, 5) (with a matching Index) then there are no gaps.
      --
      --  Another case in which there are are no gaps is when all but the last
      --  axis have a shape equal to 1. For example if the index is
      --  ((2, 2), (7, 9)), which has the shape (1, 3).
      if Is_Equal (Object.Shape, Full_Value, 1)
        or else (for all D in Full_Value'First .. Full_Value'Last - 1 => Full_Value (D) = 1)
      then
         declare
            Start_Index : Tensor_Index (Full_Index'Range);
            Stop_Index  : Tensor_Index (Full_Index'Range);
         begin
            for Axis in Full_Index'Range loop
               Start_Index (Axis) := Full_Index (Axis).Start;
               Stop_Index (Axis)  := Full_Index (Axis).Stop;
            end loop;

            declare
               Flat_Start_Index : constant Index_Type := To_Index (Start_Index, Object.Shape);
               Flat_Stop_Index  : constant Index_Type := To_Index (Stop_Index, Object.Shape);

               Count : constant Natural := Value.Elements;
               pragma Assert (Flat_Stop_Index - Flat_Start_Index + 1 = Count);
            begin
               Object.Materialize_Tensor;
               Value.Materialize_Tensor;

               Value.Reference.Data.Copy_Data
                 (Object.Reference.Data.all, 0, Flat_Start_Index - 1, Count);
            end;
         end;
      else
         raise Not_Implemented_Yet;  --  FIXME
      end if;
   end Set;

   function Flattened_Index (Object : GPU_Tensor; Index : Tensor_Index) return Index_Type is
   begin
      for Axis in Index'Range loop
         declare
            Index_Dim : constant Natural := Index (Axis);
            Shape_Dim : constant Natural := Object.Shape (Axis);
         begin
            if Index_Dim > Shape_Dim then
               raise Constraint_Error with
                 "Index (" & Trim (Index_Dim) & ") out of bounds (1 .. " & Trim (Shape_Dim) & ")";
            end if;
         end;
      end loop;

      return To_Index (Index, Object.Shape);
   end Flattened_Index;

   overriding procedure Set
     (Object : in out GPU_Tensor;
      Index  : Index_Type;
      Value  : Element) renames Operations.Set;

   overriding procedure Set
     (Object : in out GPU_Tensor;
      Index  : Index_Type;
      Value  : Boolean) renames Operations.Set;

   overriding procedure Set (Object : in out GPU_Tensor; Index : Tensor_Index; Value : Element) is
      Offset : constant Natural := Natural (Flattened_Index (Object, Index) - 1);
   begin
      Object.Materialize_Tensor;

      case Element'Size is
         when 32 =>
            Object.Reference.Data.Set_Data (Float_32_Array'(1 => Float_32 (Value)), Offset);
         when 64 =>
            Object.Reference.Data.Set_Data (Float_64_Array'(1 => Float_64 (Value)), Offset);
         when others =>
            raise Constraint_Error with "Element_Type'Size must be 32 or 64";
      end case;
   end Set;

   overriding procedure Set (Object : in out GPU_Tensor; Index : Tensor_Index; Value : Boolean) is
      Offset : constant Natural := Natural (Flattened_Index (Object, Index) - 1);
   begin
      Object.Materialize_Tensor;
      Object.Reference.Data.Set_Data (Unsigned_32_Array'(1 => (if Value then 1 else 0)), Offset);
   end Set;

   function Get (Data : Orka.Rendering.Buffers.Buffer; Offset : Natural) return Element is
   begin
      case Element'Size is
         when 32 =>
            declare
               Result : Float_32_Array (1 .. 1);
            begin
               Data.Get_Data (Result, Offset);
               return Element (Result (Result'First));
            end;
         when 64 =>
            declare
               Result : Float_64_Array (1 .. 1);
            begin
               Data.Get_Data (Result, Offset);
               return Element (Result (Result'First));
            end;
         when others =>
            raise Constraint_Error with "Element_Type'Size must be 32 or 64";
      end case;
   end Get;

   function Get (Data : Orka.Rendering.Buffers.Buffer; Offset : Natural) return Integer is
      Result : Integer_32_Array (1 .. 1);
   begin
      Data.Get_Data (Result, Offset);
      return Integer (Result (Result'First));
   end Get;

   function Get (Data : Orka.Rendering.Buffers.Buffer; Offset : Natural) return Unsigned_32 is
      Result : Unsigned_32_Array (1 .. 1);
   begin
      Data.Get_Data (Result, Offset);
      return Result (Result'First);
   end Get;

   function Get (Data : Orka.Rendering.Buffers.Buffer; Offset : Natural) return Boolean is
     (Unsigned_32'(Get (Data, Offset)) = 1);

   overriding function Get (Object : GPU_Tensor; Index : Tensor_Index) return Element is
      Offset : constant Natural := Natural (Flattened_Index (Object, Index) - 1);
   begin
      Object.Materialize_Tensor;
      return Get (Object.Reference.Data.all, Offset);
   end Get;

   overriding function Get (Object : GPU_Tensor; Index : Tensor_Index) return Boolean is
      Offset : constant Natural := Natural (Flattened_Index (Object, Index) - 1);
   begin
      Object.Materialize_Tensor;
      return Get (Object.Reference.Data.all, Offset);
   end Get;

   overriding
   function Get (Object : GPU_Tensor; Index : Range_Type) return GPU_Tensor renames Operations.Get;

   overriding function Get (Object : GPU_Tensor; Index : Tensor_Range) return GPU_Tensor is
      Rows : constant Natural := Object.Rows;

      Row_Start : constant Index_Type := Index (1).Start;
      Row_Stop  : constant Index_Type := Index (1).Stop;

      Result_Rows : constant Positive := Row_Stop - Row_Start + 1;
   begin
      case Object.Axes is
         when 1 =>
            declare
               Count : constant Positive := Result_Rows;
               Shape : constant Tensor_Shape := [1 => Count];
            begin
               if Row_Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Row_Stop) & ") out of bounds (1 .. " &
                    Trim (Rows) & ")";
               end if;

               Object.Materialize_Tensor;

               return Result : constant GPU_Tensor := With_Buffer (Shape, Object.Kind) do
                  Object.Reference.Data.Copy_Data
                    (Result.Reference.Data.all, Row_Start - 1, 0, Count);
               end return;
            end;
         when 2 =>
            declare
               Columns : constant Natural :=
                 (if 2 in Object.Shape'Range then Object.Columns else 1);

               Index_Shape : constant Tensor_Shape := Shape (Index);
               Result_Columns : constant Positive :=
                 (if 2 in Index_Shape'Range then Index_Shape (2) else Columns);

               Shape : constant Tensor_Shape :=
                 (if Result_Rows = 1 then
                    [1 => Result_Columns]
                  else
                    [1 => Result_Rows, 2 => Result_Columns]);

               Column_Start : constant Index_Type :=
                 (if 2 in Index'Range then Index (2).Start else 1);
               Column_Stop  : constant Index_Type :=
                 (if 2 in Index'Range then Index (2).Stop else Columns);
            begin
               if Row_Stop > Rows then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Row_Stop) & ") out of bounds (1 .. " &
                    Trim (Rows) & ")";
               end if;

               if Column_Stop > Columns then
                  raise Constraint_Error with
                    "Stop index (" & Trim (Column_Stop) & ") out of bounds (1 .. " &
                    Trim (Columns) & ")";
               end if;

               pragma Assert (Column_Stop - Column_Start + 1 = Result_Columns);

               Object.Materialize_Tensor;

               return Result : constant GPU_Tensor := With_Buffer (Shape, Object.Kind) do
                  for I in 1 .. Result_Rows loop
                     declare
                        Result_Index : constant Natural := (I - 1) * Result_Columns;

                        Current_Row : constant Natural := Row_Start - 1 + I;
                        Base_Index  : constant Natural := (Current_Row - 1) * Columns;
                     begin
                        Object.Reference.Data.Copy_Data (Result.Reference.Data.all,
                          Base_Index + Column_Start - 1, Result_Index, Result_Columns);
                     end;
                  end loop;
               end return;
            end;
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
   end Get;

   overriding function Get (Object : GPU_Tensor; Index : GPU_Tensor) return GPU_Tensor is
      use Rendering.Buffers;
      use Rendering.Programs;
      use all type Rendering.Buffers.Indexed_Buffer_Target;
   begin
      if Object.Elements > 0 then
         Object.Materialize_Tensor;
      end if;

      if Index.Elements > 0 then
         Index.Materialize_Tensor;
      end if;

      declare
         Indices_Count : constant Positive :=
           Positive (Groups (Elements => Index.Elements, Group_Size => 4)) * 4;
         Padding_Count : constant Integer_32 := Integer_32 (Indices_Count - Index.Elements);

         Prefix_Sum : Algorithms.Prefix_Sums.Prefix_Sum'Class :=
           Algorithms.Prefix_Sums.Create_Prefix_Sum (Context.all, Location_Prefix_Sum, Indices_Count);

         Buffer_Prefix_Sum : constant Buffer := Create_Buffer
           (Flags  => (Dynamic_Storage => True, others => False),
            Kind   => Orka.Types.UInt_Type,
            Length => Prefix_Sum.Length);
      begin
         --  Copy the indices + zero the optional padding in Buffer_Prefix_Sum
         Index.Reference.Data.Copy_Data (Buffer_Prefix_Sum, 0, 0, Length => Index.Elements);
         if Padding_Count > 0 then
            Buffer_Prefix_Sum.Set_Data
              (Unsigned_32_Array'(1 .. Padding_Count => 0), Index.Elements);
         end if;

         --  Store the prefix sum of the selected indices in Index in Buffer_Prefix_Sum
         Prefix_Sum.Compute_Prefix_Sum (Buffer_Prefix_Sum);

         declare
            Sum : constant Natural :=
              Natural (Unsigned_32'(Get (Buffer_Prefix_Sum, Index.Elements - 1)));
            Last_True : constant Boolean :=
              Get (Index.Reference.Data.all, Index.Reference.Data.Length - 1);

            --  Prefix sum is exclusive; add 1 if last True (a 1) was at the
            --  last index. This trick would not be needed if the prefix sum
            --  was inclusive.
            --  See compute shader file prefix-sum.comp for an example.
            Count : constant Natural := Sum + (if Last_True then 1 else 0);

            use Orka.Rendering.Programs.Shaders;
            use all type GL.Compute.Work_Group_Kind;

            Size_X : constant Positive :=
              Largest_Group_Size (GL.Compute.Max_Compute_Work_Group_Size (Variable) (X));
         begin
            if Count = 0 then
               return Empty ([0]);
            end if;

            return Result : constant GPU_Tensor := With_Buffer ([1 => Count], Object.Kind) do
               Buffer_Prefix_Sum.Bind (Shader_Storage, 0);
               Index.Reference.Data.Bind (Shader_Storage, 1);

               Object.Reference.Data.Bind (Shader_Storage, 2);
               Result.Reference.Data.Bind (Shader_Storage, 3);

               Context.Bind_Shaders ((Compute_Shader => From (Kernels.Program_Compact_Tensor (Object.Kind)), others => Empty));

               GL.Compute.Dispatch_Compute_Group_Size
                 (Group_Size => [Integer_32 (Size_X), 1, 1],
                  X => Groups (Elements => Object.Elements, Group_Size => Size_X));
            end return;
         end;
      end;
   end Get;

   ----------------------------------------------------------------------------

   overriding
   function Image (Object : GPU_Tensor) return String is
      Row_Count : constant := 5;
      Count     : constant Natural := Object.Elements;

      Result : SU.Unbounded_String;
      Buffer : Buffer_Access renames Object.Reference.Data;

      procedure Append_Value (Index : Natural) is
      begin
         case Object.Kind is
            when Float_Type =>
               declare
                  Value : constant Element_Type := Get (Buffer.all, Index);
               begin
                  SU.Append (Result,
                    (if Value'Valid then Value'Image else "     invalid"));
               end;
            when Int_Type =>
               SU.Append (Result, Integer'(Get (Buffer.all, Index))'Image);
            when Bool_Type =>
               SU.Append (Result, "       " &
                 (if Get (Buffer.all, Index) then " True" else "False"));
         end case;
      end Append_Value;
   begin
      Object.Materialize_Tensor;
      pragma Assert (Buffer /= null);

      SU.Append (Result, "tensor([");
      case Object.Axes is
         when 1 =>
            for I in 1 .. Count loop
               declare
                  First_Element_Of_Row : constant Boolean := (I - 1) mod Row_Count = 0;
                  Last_Element_Of_Row  : constant Boolean := (I - 0) mod Row_Count = 0;
               begin
                  if First_Element_Of_Row then
                     SU.Append (Result, (if I = 1 then "" else "        "));
                  end if;
                  Append_Value (I - 1);
                  if I < Count then
                     SU.Append (Result, ",");
                     if Last_Element_Of_Row then
                        SU.Append (Result, L1.LF);
                     end if;
                  end if;
               end;
            end loop;
         when 2 =>
            declare
               Rows    : constant Natural := Object.Rows;
               Columns : constant Natural := Object.Columns;
            begin
               for I in 1 .. Rows loop
                  SU.Append (Result, (if I = 1 then "" else "        "));
                  SU.Append (Result, "[");
                  for J in 1 .. Columns loop
                     Append_Value ((I - 1) * Columns + J - 1);
                     if J < Columns then
                        SU.Append (Result, ",");
                     end if;
                  end loop;
                  SU.Append (Result, "]");
                  if I < Rows then
                     SU.Append (Result, ",");
                     SU.Append (Result, L1.LF);
                  end if;
               end loop;
            end;
         when others =>
            raise Not_Implemented_Yet;  --  FIXME
      end case;
      SU.Append (Result, "])");

      return SU.To_String (Result);
   end Image;

   overriding
   function Shape (Object : GPU_Tensor) return Tensor_Shape is (Object.Shape);

   overriding
   function Elements (Object : GPU_Tensor) return Natural is (Elements (Object.Shape));

   overriding
   function Axes (Object : GPU_Tensor) return Tensor_Axis is (Object.Axes);

   overriding
   function Empty (Shape : Tensor_Shape) return GPU_Tensor is (Without_Data (Shape));

   overriding
   function Fill (Shape : Tensor_Shape; Value : Element) return GPU_Tensor is
     (From_Constructor
        (Shape       => Shape,
         Constructor => (Kind => Fill, Value => Value)));

   overriding function Zeros (Shape : Tensor_Shape) return GPU_Tensor renames Operations.Zeros;
   overriding function Zeros (Elements : Positive)  return GPU_Tensor renames Operations.Zeros;

   overriding function Ones (Shape : Tensor_Shape) return GPU_Tensor renames Operations.Ones;
   overriding function Ones (Elements : Positive)  return GPU_Tensor renames Operations.Ones;

   overriding
   function To_Tensor (Elements : Element_Array; Shape : Tensor_Shape) return GPU_Tensor is
   begin
      return Result : constant GPU_Tensor := With_Buffer (Shape) do
         case Element'Size is
            when 32 =>
               declare
                  Data : Float_32_Array (Size (Elements'First) .. Size (Elements'Last));
               begin
                  for Index in Data'Range loop
                     Data (Index) := Float_32 (Elements (Positive (Index)));
                  end loop;
                  Result.Reference.Data.Set_Data (Data);
               end;
            when 64 =>
               declare
                  Data : Float_64_Array (Size (Elements'First) .. Size (Elements'Last));
               begin
                  for Index in Data'Range loop
                     Data (Index) := Float_64 (Elements (Positive (Index)));
                  end loop;
                  Result.Reference.Data.Set_Data (Data);
               end;
            when others =>
               raise Constraint_Error with "Element_Type'Size must be 32 or 64";
         end case;
      end return;
   end To_Tensor;

   overriding
   function To_Tensor (Elements : Element_Array) return GPU_Tensor renames Operations.To_Tensor;

   overriding
   function To_Boolean_Tensor
     (Booleans : Boolean_Array;
      Shape    : Tensor_Shape) return GPU_Tensor
   is
      Data : Unsigned_32_Array (Integer_32 (Booleans'First) .. Integer_32 (Booleans'Last));
   begin
      for Index in Data'Range loop
         Data (Index) := (if Booleans (Positive (Index)) then 1 else 0);
      end loop;

      return Result : constant GPU_Tensor := With_Buffer (Shape, Kind => Bool_Type) do
         Result.Reference.Data.Set_Data (Data);
      end return;
   end To_Boolean_Tensor;

   overriding
   function To_Boolean_Tensor (Booleans : Boolean_Array) return GPU_Tensor
     renames Operations.To_Boolean_Tensor;

   overriding
   function Linear_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed) return GPU_Tensor
   is
      Shape : constant Tensor_Shape := [1 => Count];

      Step : constant Element :=
        (if Count > 1 then (Stop - Start) / Element (Count - (case Interval is
                                                                when Closed    => 1,
                                                                when Half_Open => 0))
                      else 0.0);
   begin
      return From_Constructor
        (Shape       => Shape,
         Constructor =>
           (Kind  => Linear_Space,
            Start => Start,
            Step  => Step,
            Base  => <>));  --  'Base' is not used by Linear_Space
   end Linear_Space;

   overriding
   function Log_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return GPU_Tensor
   is
      Shape : constant Tensor_Shape := [1 => Count];

      Step : constant Element :=
        (if Count > 1 then (Stop - Start) / Element (Count - (case Interval is
                                                                when Closed    => 1,
                                                                when Half_Open => 0))
                      else 0.0);
   begin
      return From_Constructor
        (Shape       => Shape,
         Constructor =>
           (Kind  => Log_Space,
            Start => Start,
            Step  => Step,
            Base  => Base));
   end Log_Space;

   overriding
   function Geometric_Space
     (Start, Stop : Element;
      Count       : Positive;
      Interval    : Interval_Kind := Closed;
      Base        : Element := 10.0) return GPU_Tensor renames Operations.Geometric_Space;

   overriding
   function Array_Range (Start, Stop : Element; Step : Element := 1.0) return GPU_Tensor
     renames Operations.Array_Range;

   overriding
   function Array_Range (Stop : Element) return GPU_Tensor renames Operations.Array_Range;

   overriding
   function Identity (Size : Positive; Offset : Integer := 0) return GPU_Tensor
     renames Operations.Identity;

   overriding
   function Identity (Rows, Columns : Positive; Offset : Integer := 0) return GPU_Tensor is
      Shape : constant Tensor_Shape := [1 => Rows, 2 => Columns];

      Max_Size : constant Positive := Positive'Max (Rows, Columns);
   begin
      if Offset in -(Max_Size - 1) .. Max_Size - 1 then
         return From_Constructor
           (Shape       => Shape,
            Constructor => (Kind => Identity, Offset => Offset));
      else
         return Zeros (Shape);
      end if;
   end Identity;

   overriding
   function Upper_Triangular (Object : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor
     renames Operations.Upper_Triangular;

   overriding
   function Main_Diagonal (Object : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor is
      Rows    : constant Positive := Object.Rows;
      Columns : constant Positive := Object.Columns;

      Shape : constant Tensor_Shape := [1 => Positive'Min (Rows, Columns)];
   begin
      return From_Matrix_Operation
        (Shape     => Shape,
         Operation => (Kind   => Main_Diagonal,
                       Value  => Tensor_Holders.To_Holder (Object),
                       Offset => Offset));
   end Main_Diagonal;

   overriding
   function Diagonal (Elements : Element_Array; Offset : Integer := 0) return GPU_Tensor is
      Size : constant Positive := Elements'Length;

      Shape : constant Tensor_Shape := [1 .. 2 => Size];
   begin
      if Offset in -(Size - 1) .. Size - 1 then
         return Diagonal (To_Tensor (Elements), Offset);
      else
         return Zeros (Shape);
      end if;
   end Diagonal;

   overriding
   function Diagonal (Elements : GPU_Tensor; Offset : Integer := 0) return GPU_Tensor is
      Size : constant Positive := Elements.Elements;

      Shape : constant Tensor_Shape := [1 .. 2 => Size];
   begin
      if Offset in -(Size - 1) .. Size - 1 then
         return From_Matrix_Operation
           (Shape     => Shape,
            Operation => (Kind   => Diagonal,
                          Value  => Tensor_Holders.To_Holder (Elements),
                          Offset => Offset));
      else
         return Zeros (Shape);
      end if;
   end Diagonal;

   overriding
   function Trace (Object : GPU_Tensor; Offset : Integer := 0) return Element
     renames Operations.Trace;

   overriding
   function Reshape (Object : GPU_Tensor; Shape : Tensor_Shape) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Shape,
         Kind     => Object.Kind,
         Operator => Reshape,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding
   function Reshape (Object : GPU_Tensor; Elements : Positive) return GPU_Tensor
     renames Operations.Reshape;

   overriding
   function Flatten (Object : GPU_Tensor) return GPU_Tensor renames Operations.Flatten;

   overriding
   function Concatenate
     (Left, Right : GPU_Tensor;
      Axis        : Tensor_Axis) return GPU_Tensor
   is
      Shape : constant Tensor_Shape := Add (Left.Shape, Right.Shape, Axis);
      pragma Assert (Elements (Shape) = Left.Elements + Right.Elements);
   begin
      if Left.Elements > 0 then
         Left.Materialize_Tensor;
      end if;

      if Right.Elements > 0 then
         Right.Materialize_Tensor;
      end if;

      if Left.Elements = 0 then
         return Right;
      elsif Right.Elements = 0 then
         return Left;
      end if;

      return Result : constant GPU_Tensor := With_Buffer (Shape, Left.Kind) do
         case Axis is
            when 1 =>
               Left.Reference.Data.Copy_Data
                 (Result.Reference.Data.all, 0, 0, Left.Elements);
               Right.Reference.Data.Copy_Data
                 (Result.Reference.Data.all, 0, Left.Elements, Right.Elements);
            when 2 =>
               declare
                  Rows : constant Positive := Left.Rows;

                  Columns_Left  : constant Positive := Left.Columns;
                  Columns_Right : constant Positive := Right.Columns;
               begin
                  --  TODO It may or may not be faster to use a CS to copy Left and Right
                  for Index in 1 .. Rows loop
                     declare
                        Left_Offset : constant Natural :=
                          (Index - 1) * (Columns_Left + Columns_Right);
                        Right_Offset : constant Natural := Left_Offset + Columns_Left;

                        Left_Index  : constant Natural := (Index - 1) * Columns_Left;
                        Right_Index : constant Natural := (Index - 1) * Columns_Right;
                     begin
                        Left.Reference.Data.Copy_Data
                          (Result.Reference.Data.all, Left_Index, Left_Offset, Columns_Left);
                        Right.Reference.Data.Copy_Data
                          (Result.Reference.Data.all, Right_Index, Right_Offset, Columns_Right);
                     end;
                  end loop;
               end;
            when others =>
               raise Not_Implemented_Yet;  --  FIXME
         end case;
      end return;
   end Concatenate;

   overriding
   function "&" (Left, Right : GPU_Tensor) return GPU_Tensor renames Operations."&";

   ----------------------------------------------------------------------------
   --                            Matrix operations                           --
   ----------------------------------------------------------------------------

   overriding
   function "*" (Left, Right : GPU_Tensor) return GPU_Tensor is
      --  m x n * n x p
      --      ^   ^
      --      |___|
      Left_Rows     : constant Natural := (if Left.Axes = 2 then Left.Rows else 1);
      Right_Columns : constant Natural := (if Right.Axes = 2 then Right.Columns else 1);

      Shape : constant Tensor_Shape :=
         (case Right.Axes is
            when 1 => [1 => Left_Rows],
            when 2 => [1 => Left_Rows, 2 => Right_Columns],
            when others => raise Not_Implemented_Yet);  --  FIXME
   begin
      --  Matrix-matrix, matrix-vector, or vector-matrix multiplication
      return From_Matrix_Operation
        (Shape     => Shape,
         Operation => (Kind  => Matrix_Matrix,
                       Left  => Tensor_Holders.To_Holder (Left),
                       Right => Tensor_Holders.To_Holder (Right)));
   end "*";

   overriding
   function "*" (Left, Right : GPU_Tensor) return Element is
      Result : constant GPU_Tensor := Left.Reshape ([1, Left.Elements]) * Right;
   begin
      return Result (1);
   end "*";

   overriding function "**" (Left : GPU_Tensor; Right : Integer) return GPU_Tensor
     renames Operations."**";

   overriding
   function Outer (Left, Right : GPU_Tensor) return GPU_Tensor is
     (Left.Reshape ([Left.Elements, 1]) * Right.Reshape ([1, Right.Elements]));

   overriding
   function Inverse (Object : GPU_Tensor) return GPU_Tensor renames Operations.Inverse;

   overriding
   function Transpose (Object : GPU_Tensor) return GPU_Tensor is
      Shape : constant Tensor_Shape :=
        [1 => Object.Columns,
         2 => Object.Rows];
   begin
      return From_Matrix_Operation
        (Shape     => Shape,
         Operation => (Kind   => Transpose,
                       Value  => Tensor_Holders.To_Holder (Object),
                       Offset => <>));  --  'Offset' is not used by Transpose
   end Transpose;

   ----------------------------------------------------------------------------

   overriding
   function Solve (A, B : GPU_Tensor; Solution : out Solution_Kind) return GPU_Tensor
     renames Operations.Solve;

   overriding
   function Solve (A, B : GPU_Tensor; Form : Triangular_Form) return GPU_Tensor
     renames Operations.Solve;

   overriding
   function Divide_By (B, A : GPU_Tensor) return GPU_Tensor
     renames Operations.Divide_By;

   overriding
   function Divide_By (B, A : GPU_Tensor; Form : Triangular_Form) return GPU_Tensor
     renames Operations.Divide_By;

   overriding
   function QR (Object : GPU_Tensor) return GPU_Tensor
     renames Operations.QR;

   overriding
   function QR (Object : GPU_Tensor; Mode : QR_Mode := Reduced) return QR_Factorization'Class
     renames Operations.QR;

   overriding
   function QR_For_Least_Squares (Object : GPU_Tensor) return QR_Factorization'Class
     renames Operations.QR_For_Least_Squares;

   overriding
   function Least_Squares (Object : QR_Factorization'Class; B : GPU_Tensor) return GPU_Tensor
     renames Operations.Least_Squares;

   overriding
   function Least_Squares (A, B : GPU_Tensor) return GPU_Tensor
     renames Operations.Least_Squares;

   overriding
   function Constrained_Least_Squares (A, B, C, D : GPU_Tensor) return GPU_Tensor
     renames Operations.Constrained_Least_Squares;

   overriding
   function Cholesky (Object : GPU_Tensor; Form : Triangular_Form := Lower) return GPU_Tensor
     renames Operations.Cholesky;

   overriding
   function Cholesky_Update
     (R, V : GPU_Tensor;
      Mode : Update_Mode) return GPU_Tensor renames Operations.Cholesky_Update;

   ----------------------------------------------------------------------------
   --                            Vector operations                           --
   ----------------------------------------------------------------------------

   overriding
   function Norm (Object : GPU_Tensor) return Element renames Operations.Norm;

   overriding
   function Normalize (Object : GPU_Tensor) return GPU_Tensor renames Operations.Normalize;

   overriding
   function Standardize (Object : GPU_Tensor) return GPU_Tensor renames Operations.Standardize;

   overriding
   function Correlation_Coefficient (Left, Right : GPU_Tensor) return Correlation_Element
     renames Operations.Correlation_Coefficient;

   ----------------------------------------------------------------------------
   --                         Element-wise operations                        --
   ----------------------------------------------------------------------------

   overriding function "+" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Add,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "-" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Subtract,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   --  Note: Element-wise function "*" (Left, Right : GPU_Tensor) is called Multiply

   overriding function "/" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Divide,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function Divide_Or_Zero (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Divide_Or_Zero,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "**" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Power,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "**" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
   begin
      if Right = 0.0 then
         return Ones (Left.Shape);
      elsif Right = 1.0 then
         return Left;
      else
         return From_Binary_Operation
           (Shape    => Left.Shape,
            Operator => Power,
            Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
            Right    => (Kind => Scalar_Value, Value  => Right));
      end if;
   end "**";

   overriding function "**" (Left : Element; Right : GPU_Tensor) return GPU_Tensor is
   begin
      if Left = 1.0 then
         return Ones (Right.Shape);
      else
         return From_Binary_Operation
           (Shape    => Right.Shape,
            Operator => Power,
            Left     => (Kind => Scalar_Value, Value  => Left),
            Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right)));
      end if;
   end "**";

   overriding function "*" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Multiply,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function "/" (Left : Element; Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Right.Shape,
         Operator => Divide,
         Left     => (Kind => Scalar_Value, Value  => Left),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "/" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Divide,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function "+" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Add,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function "-" (Left : Element; Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Right.Shape,
         Operator => Subtract,
         Left     => (Kind => Scalar_Value, Value  => Left),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "-" (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Minus,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding
   function "*" (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."*";

   overriding
   function "+" (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."+";

   overriding
   function "-" (Left : GPU_Tensor; Right : Element) return GPU_Tensor renames Operations."-";

   overriding
   function "mod" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Modulus,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding
   function "rem" (Left, Right : GPU_Tensor) return GPU_Tensor renames Operations."rem";

   overriding
   function "mod" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Modulus,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding
   function "rem" (Left : GPU_Tensor; Right : Element) return GPU_Tensor renames Operations."rem";

   overriding function "abs" (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Absolute,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Multiply (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Multiply,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding
   function Power (Left : GPU_Tensor; Right : Integer) return GPU_Tensor renames Operations.Power;

   overriding
   function Min (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations.Min;

   overriding
   function Max (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations.Max;

   overriding function Min (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Min,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function Max (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Max,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function Sqrt (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Sqrt,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Ceil (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Ceil,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Floor (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Floor,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Round (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Round,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Truncate (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Truncate,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Exp (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Exp,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Log (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Log,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Log10 (Object : GPU_Tensor) return GPU_Tensor renames Operations.Log10;

   overriding function Log2 (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Log2,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   ----------------------------------------------------------------------------
   --                              Trigonometry                              --
   ----------------------------------------------------------------------------

   overriding function Sin (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Sin,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Cos (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Cos,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Tan (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Tan,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Arcsin (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Arcsin,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Arccos (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Operator => Arccos,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   overriding function Arctan (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Arctan,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function Degrees (Object : GPU_Tensor) return GPU_Tensor renames Operations.Degrees;

   overriding function Radians (Object : GPU_Tensor) return GPU_Tensor renames Operations.Radians;

   ----------------------------------------------------------------------------
   --                               Reductions                               --
   ----------------------------------------------------------------------------

   function Apply_Reduction
     (Object      : GPU_Tensor;
      Subject     : Expression'Class;
      Initial     : Element;
      Associative : Boolean) return Element
   is
      use Rendering.Buffers;
      use Rendering.Programs;

      Left_Literal : constant Expressions.Expression_String :=
        Expressions.Value ("left");
      Right_Literal : constant Expressions.Expression_String :=
        Expressions.Value ("right");

      function Get_Kernel
        (Buffer_Constants : aliased in out Buffer;
         Work_Group_Size  : Positive) return Shader_Program
      is
         GPU_Subject : constant Expression_Type := Expression_Type (Subject);

         Full_Expression : constant Expressions.Expression_String :=
           Apply_With_Constants_Buffer
             (Buffer_Constants, GPU_Subject, Left_Literal, Right_Literal);

         function Get_Program
           (Programs : in out Reduction_Program_Vectors.Vector) return Rendering.Programs.Shader_Program
         is
            Hash : constant Ada.Containers.Hash_Type :=
              Ada.Strings.Hash (Expressions.Image (Full_Expression));

            Index_Program : Natural;
            Found_Program : Boolean := False;

            procedure Find_Index (Elements : Reduction_Program_Vectors.Element_Array) is
               use type Ada.Containers.Hash_Type;
            begin
               for Index in Elements'Range loop
                  if Elements (Index).Hash = Hash and
                     Elements (Index).Size = Work_Group_Size
                  then
                     Index_Program := Index;
                     Found_Program := True;
                     exit;
                  end if;
               end loop;
            end Find_Index;
         begin
            Programs.Query (Find_Index'Access);

            if Found_Program then
               return Programs (Index_Program).Program;
            end if;

            declare
               Source : SU.Unbounded_String := (if Work_Group_Size > 1 then
                                                  Kernels.Source_Reduce_Assoc
                                                else
                                                  Kernels.Source_Reduce);
               Text : constant String := "value = " & Expressions.Image (Full_Expression) & ";";
            begin
               Strings.Replace (Source, "%DATA_TYPE%", Data_Type_Image (Object.Kind));
               Strings.Replace (Source, "%OPERATIONS%", Text);
               Strings.Replace (Source, "%LOCAL_GROUP_SIZE%", Work_Group_Size'Image);

               return Result : constant Shader_Program := Create_Program_From_Source (Compute_Shader, +Source) do
                  Programs.Append ((Work_Group_Size, Hash, Result));
               end return;
            end;
         end Get_Program;
      begin
         return Get_Program (Kernels.Reduction_Programs);
      end Get_Kernel;
   begin
      if Object.Elements = 0 then
         return Initial;
      end if;

      Object.Materialize_Tensor;

      declare
         use all type GL.Compute.Work_Group_Kind;

         Size_X : constant Positive :=
           (if Associative then
              Largest_Group_Size (GL.Compute.Max_Compute_Work_Group_Size (Variable) (X))
            else
              1);

         Buffer_Constants : aliased Buffer := Create_Buffer
           (Flags  => (Dynamic_Storage => True, others => False),
            Kind   => From_Kind (Object.Kind),
            Length => Max_Reduction_Constants);

         Kernel : constant Shader_Program := Get_Kernel (Buffer_Constants, Size_X);
         Uniform_Identity : constant Uniforms.Uniform := Kernel.Uniform ("identity_value");

         use Orka.Rendering.Programs.Shaders;
      begin
         case Element'Size is
            when 32     => Uniform_Identity.Set_Single (Float_32 (Initial));
            when 64     => Uniform_Identity.Set_Double (Float_64 (Initial));
            when others => raise Constraint_Error with "Element_Type'Size must be 32 or 64";
         end case;

         Context.Bind_Shaders ((Compute_Shader => From (Kernel), others => Empty));

         declare
            function Reduce (Buffer_Input : Buffer) return Buffer is
               Work_Groups : constant Positive :=
                 (if Associative then
                    Positive (Groups (Elements => Buffer_Input.Length, Group_Size => Size_X))
                  else
                    1);
            begin
               return Buffer_Output : constant Buffer := Create_Buffer
                 (Flags  => (others => False),
                  Kind   => From_Kind (Object.Kind),
                  Length => Work_Groups)
               do
                  Buffer_Input.Bind (Shader_Storage, 0);
                  Buffer_Output.Bind (Shader_Storage, 1);
                  Buffer_Constants.Bind (Shader_Storage, 2);

                  GL.Barriers.Memory_Barrier
                    ((Shader_Storage => True, others => False));

                  if Associative then
                     GL.Compute.Dispatch_Compute_Group_Size
                       (Group_Size => [Integer_32 (Size_X), 1, 1], X => Unsigned_32 (Work_Groups));
                  else
                     pragma Assert (Work_Groups = 1);
                     GL.Compute.Dispatch_Compute
                       (X => Unsigned_32 (Work_Groups));
                  end if;
               end return;
            end Reduce;

            Buffer_Input : Buffer := Object.Reference.Data.all;
         begin
            loop
               declare
                  Buffer_Output : constant Buffer := Reduce (Buffer_Input);
               begin
                  if Buffer_Output.Length = 1 then
                     GL.Barriers.Memory_Barrier
                       ((Buffer_Update | Shader_Storage => True, others => False));

                     return Get (Buffer_Output, 0);
                  end if;

                  Buffer_Input := Buffer_Output;
               end;
            end loop;
         end;
      end;
   end Apply_Reduction;

   overriding
   function Reduce_Associative
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element) return Element is
   begin
      return Apply_Reduction (Object, Subject, Initial, Associative => True);
   end Reduce_Associative;

   overriding
   function Reduce_Associative
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return GPU_Tensor is
   begin
      raise Not_Implemented_Yet;  --  FIXME
      return Zeros ([1]);
   end Reduce_Associative;

   overriding
   function Reduce
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element) return Element is
   begin
      return Apply_Reduction (Object, Subject, Initial, Associative => False);
   end Reduce;

   overriding
   function Reduce
     (Object  : GPU_Tensor;
      Subject : Expression'Class;
      Initial : Element;
      Axis    : Tensor_Axis) return GPU_Tensor is
   begin
      raise Not_Implemented_Yet;  --  FIXME
      return Zeros ([1]);
   end Reduce;

   overriding function Sum (Object : GPU_Tensor) return Element renames Operations.Sum;

   overriding function Product (Object : GPU_Tensor) return Element renames Operations.Product;

   overriding
   function Sum (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Sum;

   overriding
   function Product (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Product;

   ----------------------------------------------------------------------------
   --                               Statistics                               --
   ----------------------------------------------------------------------------

   overriding function Min (Object : GPU_Tensor) return Element renames Operations.Min;

   overriding function Max (Object : GPU_Tensor) return Element renames Operations.Max;

   overriding function Quantile (Object : GPU_Tensor; P : Probability) return Element
     renames Operations.Quantile;

   overriding function Median (Object : GPU_Tensor) return Element
     renames Operations.Median;

   overriding function Mean (Object : GPU_Tensor) return Element
     renames Operations.Mean;

   overriding
   function Variance (Object : GPU_Tensor; Offset : Natural := 0) return Element
     renames Operations.Variance;

   overriding
   function Standard_Deviation (Object : GPU_Tensor; Offset : Natural := 0) return Element
     renames Operations.Standard_Deviation;

   overriding
   function Min (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Min,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding
   function Max (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Operator => Max,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding
   function Min (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Min;

   overriding
   function Max (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Max;

   overriding
   function Quantile
     (Object : GPU_Tensor;
      P      : Probability;
      Axis   : Tensor_Axis) return GPU_Tensor is
   begin
      raise Not_Implemented_Yet;  --  FIXME
      return Zeros ([1]);
   end Quantile;

   overriding
   function Mean (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Mean;

   overriding
   function Variance
     (Object : GPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return GPU_Tensor
   renames Operations.Variance;

   overriding
   function Median (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor
     renames Operations.Median;

   overriding
   function Standard_Deviation
     (Object : GPU_Tensor;
      Axis   : Tensor_Axis;
      Offset : Natural := 0) return GPU_Tensor
   renames Operations.Standard_Deviation;

   ----------------------------------------------------------------------------
   --                                Logical                                 --
   ----------------------------------------------------------------------------

   overriding function And_Not (Left, Right : GPU_Tensor) return GPU_Tensor
     renames Operations.And_Not;

   overriding function "and" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Left.Kind,
         Operator => Logical_And,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));
   --  Note: function "and" supports Float_Type and Bool_Type

   overriding function "and" (Left : Element; Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Right.Shape,
         Kind     => Float_Type,
         Operator => Logical_And,
         Left     => (Kind => Scalar_Value, Value  => Left),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "or"  (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Logical_Or,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "xor" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Logical_Xor,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "not"  (Object : GPU_Tensor) return GPU_Tensor is
     (From_Unary_Operation
        (Shape    => Object.Shape,
         Kind     => Bool_Type,
         Operator => Logical_Not,
         Value    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Object))));

   ----------------------------------------------------------------------------
   --                              Comparisons                               --
   ----------------------------------------------------------------------------

   overriding
   function "="  (Left : GPU_Tensor; Right : Element) return GPU_Tensor renames Operations."=";

   overriding
   function "/=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor renames Operations."/=";

   overriding function ">"  (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Greater_Than,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function "<"  (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Less_Than,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function ">=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Greater_Equal,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   overriding function "<=" (Left : GPU_Tensor; Right : Element) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Less_Equal,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Scalar_Value, Value  => Right)));

   ----------------------------------------------------------------------------

   overriding
   function "="  (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."=";

   overriding
   function "/=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."/=";

   overriding
   function ">"  (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations.">";

   overriding
   function "<"  (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."<";

   overriding
   function ">=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations.">=";

   overriding
   function "<=" (Left : Element; Right : GPU_Tensor) return GPU_Tensor renames Operations."<=";

   ----------------------------------------------------------------------------

   overriding function "=" (Left, Right : GPU_Tensor) return Boolean renames Operations."=";

   overriding function "=" (Left, Right : GPU_Tensor) return GPU_Tensor is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) <= Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            return From_Binary_Operation
              (Shape    => Left.Shape,
               Kind     => Bool_Type,
               Operator => Equal,
               Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
               Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right)));
      end case;
   end "=";

   overriding function "/=" (Left, Right : GPU_Tensor) return GPU_Tensor is
   begin
      case Left.Kind is
         when Float_Type =>
            return (abs (Left - Right) > Element_Type'Model_Epsilon);
         when Int_Type | Bool_Type =>
            return From_Binary_Operation
              (Shape    => Left.Shape,
               Kind     => Bool_Type,
               Operator => Not_Equal,
               Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
               Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right)));
      end case;
   end "/=";

   overriding function ">"  (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Greater_Than,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "<"  (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Less_Than,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function ">=" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Greater_Equal,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   overriding function "<=" (Left, Right : GPU_Tensor) return GPU_Tensor is
     (From_Binary_Operation
        (Shape    => Left.Shape,
         Kind     => Bool_Type,
         Operator => Less_Equal,
         Left     => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Left)),
         Right    => (Kind => Tensor_Value, Tensor => Tensor_Holders.To_Holder (Right))));

   ----------------------------------------------------------------------------

   overriding
   function All_Close
     (Left, Right        : GPU_Tensor;
      Relative_Tolerance : Element := 1.0e-05;
      Absolute_Tolerance : Element := Element_Type'Model_Epsilon) return Boolean
   renames Operations.All_Close;

   overriding
   function Any_True (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor is
   begin
      raise Not_Implemented_Yet;  --  FIXME
      return Zeros ([1]);
   end Any_True;

   overriding
   function Any_True (Object : GPU_Tensor) return Boolean is
      Result : constant GPU_Tensor :=
        From_Matrix_Operation
          (Shape     => [1 => 1],
           Kind      => Bool_Type,
           Operation => (Kind   => Any_True,
                         Value  => Tensor_Holders.To_Holder (Object),
                         Offset => <>));  --  'Offset' is not used by Any_True
   begin
      return Result (1);
   end Any_True;

   overriding
   function All_True (Object : GPU_Tensor; Axis : Tensor_Axis) return GPU_Tensor is
   begin
      raise Not_Implemented_Yet;  --  FIXME
      return Zeros ([1]);
   end All_True;

   overriding
   function All_True (Object : GPU_Tensor) return Boolean is
      Result : constant GPU_Tensor :=
        From_Matrix_Operation
          (Shape     => [1 => 1],
           Kind      => Bool_Type,
           Operation => (Kind   => All_True,
                         Value  => Tensor_Holders.To_Holder (Object),
                         Offset => <>));  --  'Offset' is not used by All_True
   begin
      return Result (1);
   end All_True;

   procedure Reset_Random (Seed : Duration) is
      Value : constant Unsigned_32 := Unsigned_32 (Unsigned_64 (Seed) mod Unsigned_32'Modulus);

      function Rotate_Left (X : Unsigned_32; K : Natural) return Unsigned_32 is
        ((X * 2**K) or (X / 2**(Unsigned_32'Size - K)));
   begin
      Random_State :=
        [Rotate_Left (Value, 1),
         Rotate_Left (Value, 2)];
   end Reset_Random;

   overriding function Random_Uniform (Shape : Tensor_Shape) return GPU_Tensor is
     (From_Matrix_Operation
        (Shape     => Shape,
         Operation => (Kind => Random)));

end Orka.Numerics.Tensors.CS_GPU;
