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

package Orka.Buffers.MDI is
   pragma Preelaborate;

   type MDI_Buffers is record
      Vertex_Buffer, Index_Buffer, Command_Buffer : Buffer;
   end record;

   type MDI_Constructor (Max_Parts, Vertex_Length : Size) is tagged limited private;

   procedure Add_Part (Object : in out MDI_Constructor;
                       Vertices : not null Indirect.Single_Array_Access;
                       Indices  : not null Indirect.UInt_Array_Access;
                       Instance_Index : out Natural)
     with Pre  => Size (Object.Parts) < Object.Max_Parts
                    and Indices'Length > 0 and Vertices'Length > 0
                    and (Vertices'Length mod Object.Vertex_Length) = 0,
          Post => Size (Instance_Index) < Object.Max_Parts;

   function Parts (Object : MDI_Constructor) return Natural
     with Inline;

   function Create_Buffer (Object : MDI_Constructor;
                           Usage   : GL.Objects.Buffers.Buffer_Usage;
                           Visible : Boolean := True) return MDI_Buffers
     with Pre => Object.Parts > 0;

   function Create_Constructor (Max_Parts, Vertex_Length : Size) return MDI_Constructor
     with Pre => Max_Parts > 0 and Vertex_Length > 0;

private

   type Vertices_Array is array (Size range <>) of Indirect.Single_Array_Access;
   type Indices_Array  is array (Size range <>) of Indirect.UInt_Array_Access;

   type MDI_Constructor (Max_Parts, Vertex_Length : Size) is tagged limited record
      Count : Natural;
      Parts_Vertices : Vertices_Array (Size range 1 .. Max_Parts);
      Parts_Indices  : Indices_Array  (Size range 1 .. Max_Parts);
   end record;

end Orka.Buffers.MDI;
