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

private with GL.Low_Level;

package GL.Barriers is
   pragma Preelaborate;

   type Memory_Barrier_Bits (By_Region : Boolean) is record
      Uniform              : Boolean := False;
      Texture_Fetch        : Boolean := False;
      Shader_Image_Access  : Boolean := False;
      Framebuffer          : Boolean := False;
      Atomic_Counter       : Boolean := False;
      Shader_Storage       : Boolean := False;
      case By_Region is
         when False =>
            Vertex_Attrib_Array  : Boolean := False;
            Element_Array        : Boolean := False;
            Command              : Boolean := False;
            Pixel_Buffer         : Boolean := False;
            Texture_Update       : Boolean := False;
            Buffer_Update        : Boolean := False;
            Transform_Feedback   : Boolean := False;
            Client_Mapped_Buffer : Boolean := False;
            Query_Buffer         : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   procedure Texture_Barrier;

   procedure Memory_Barrier (Bits : Memory_Barrier_Bits)
     with Pre => not Bits.By_Region;
   --  Order memory transactions issued before this call relative
   --  to those issues after this call

   procedure Memory_Barrier_By_Region (Bits : Memory_Barrier_Bits)
     with Pre => Bits.By_Region;
   --  Order memory transactions caused by fragment shaders

private

   for Memory_Barrier_Bits use record
      Vertex_Attrib_Array  at 0 range 0 .. 0;
      Element_Array        at 0 range 1 .. 1;
      Uniform              at 0 range 2 .. 2;
      Texture_Fetch        at 0 range 3 .. 3;
      Shader_Image_Access  at 0 range 5 .. 5;
      Command              at 0 range 6 .. 6;
      Pixel_Buffer         at 0 range 7 .. 7;
      Texture_Update       at 0 range 8 .. 8;
      Buffer_Update        at 0 range 9 .. 9;
      Framebuffer          at 0 range 10 .. 10;
      Transform_Feedback   at 0 range 11 .. 11;
      Atomic_Counter       at 0 range 12 .. 12;
      Shader_Storage       at 0 range 13 .. 13;
      Client_Mapped_Buffer at 0 range 14 .. 14;
      Query_Buffer         at 0 range 15 .. 15;

      -- Discriminant
      By_Region            at 2 range 0 .. 0;
   end record;
   for Memory_Barrier_Bits'Size use Low_Level.Bitfield'Size;

end GL.Barriers;
