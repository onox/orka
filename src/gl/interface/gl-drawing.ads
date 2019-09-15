--  SPDX-License-Identifier: Apache-2.0
--
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

with GL.Types;

package GL.Drawing is
   pragma Preelaborate;

   use GL.Types;

   procedure Draw_Arrays
     (Mode          : Connection_Mode;
      Offset, Count : Size;
      Instances     : Size := 1;
      Base_Instance : Size := 0);

   procedure Draw_Multiple_Arrays_Indirect
     (Mode   : Connection_Mode;
      Count  : Size;
      Offset : Size := 0);

   procedure Draw_Multiple_Arrays_Indirect_Count
     (Mode      : Connection_Mode;
      Max_Count : Size;
      Offset, Count_Offset : Size := 0);

   procedure Draw_Elements
     (Mode          : Connection_Mode;
      Count         : Size;
      Index_Kind    : Index_Type;
      Index_Offset  : Natural;
      Instances     : Size := 1;
      Base_Instance : Size := 0;
      Base_Vertex   : Size := 0);

   procedure Draw_Multiple_Elements_Indirect
     (Mode       : Connection_Mode;
      Index_Kind : Index_Type;
      Count      : Size;
      Offset     : Size := 0);

   procedure Draw_Multiple_Elements_Indirect_Count
     (Mode         : Connection_Mode;
      Index_Kind   : Index_Type;
      Max_Count    : Size;
      Offset, Count_Offset : Size := 0);

end GL.Drawing;
