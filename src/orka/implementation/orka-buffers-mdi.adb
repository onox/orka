--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

package body Orka.Buffers.MDI is

   function Create_Constructor (Max_Parts, Vertex_Length : Size) return MDI_Constructor is
   begin
      return (Max_Parts => Max_Parts,
              Vertex_Length => Vertex_Length,
              Count => 0,
              Parts_Vertices => (others => null),
              Parts_Indices  => (others => null));
   end Create_Constructor;

   procedure Add_Part (Object : in out MDI_Constructor;
                       Vertices : not null Indirect.Single_Array_Access;
                       Indices  : not null Indirect.UInt_Array_Access;
                       Instance_Index : out Natural) is
   begin
      Instance_Index := Object.Count;

      Object.Count := Object.Count + 1;
      Object.Parts_Vertices (Size (Object.Count)) := Vertices;
      Object.Parts_Indices  (Size (Object.Count)) := Indices;
   end Add_Part;

   function Parts (Object : MDI_Constructor) return Natural
     is (Object.Count);

   function Create_Buffer (Object : MDI_Constructor;
                           Usage   : GL.Objects.Buffers.Buffer_Usage;
                           Visible : Boolean := True) return MDI_Buffers
   is
      Commands  : Indirect.Elements_Indirect_Command_Array (1 .. Int (Object.Count));
      Instances : constant UInt := (if Visible then 1 else 0);

      VBO_Length, IBO_Length : Size := 0;
   begin
      --  Compute size of vertex and index buffers
      for I in Commands'Range loop
         VBO_Length := VBO_Length + Object.Parts_Vertices (I)'Length;
         IBO_Length := IBO_Length + Object.Parts_Indices  (I)'Length;
      end loop;

      declare
         Vertices : Single_Array (0 .. VBO_Length - 1);
         Indices  : UInt_Array   (0 .. IBO_Length - 1);

         Part_Vertex_Count, Part_Index_Count : Size;
         Vertex_Offset, Index_Offset : Size := 0;

         VA_First, VA_Last : Size := 0;
         IA_First, IA_Last : Size := 0;
      begin
         for I in Commands'Range loop
            Part_Vertex_Count := Object.Parts_Vertices (I)'Length / Object.Vertex_Length;
            Part_Index_Count  := Object.Parts_Indices  (I)'Length;

            VA_First := Vertex_Offset * Object.Vertex_Length;
            VA_Last  := VA_First + Part_Vertex_Count * Object.Vertex_Length - 1;

            IA_First := Index_Offset;
            IA_Last  := IA_First + Part_Index_Count - 1;

            --  Copy part data to a slice of the vertices and indices arrays
            Vertices (VA_First .. VA_Last) := Object.Parts_Vertices (I).all;
            Indices  (IA_First .. IA_Last) := Object.Parts_Indices (I).all;

            --  Create draw command
            Commands (I) := (Count         => UInt (Part_Index_Count),
                             Instances     => Instances,
                             First_Index   => UInt (Index_Offset),
                             Base_Vertex   => UInt (Vertex_Offset),
                             Base_Instance => UInt (I - Commands'First));

            Vertex_Offset := Vertex_Offset + Part_Vertex_Count;
            Index_Offset  := Index_Offset  + Part_Index_Count;
         end loop;

         pragma Assert (Vertices'Last = VA_Last);
         pragma Assert (Indices'Last  = IA_Last);

         return Result : MDI_Buffers := (others => Orka.Buffers.Create_Buffer (Usage)) do
            Result.Vertex_Buffer.Set_Data  (Vertices);
            Result.Index_Buffer.Set_Data   (Indices);
            Result.Command_Buffer.Set_Data (Commands);
         end return;
      end;
   end Create_Buffer;

end Orka.Buffers.MDI;
