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

with EGL.API;
with EGL.Errors;

package body EGL.Objects.Configs is

   Config_Color_Buffer_Type : constant Int := 16#303F#;
   Config_RGB_Buffer        : constant Int := 16#308E#;
   Config_Caveat            : constant Int := 16#3027#;
   Config_Conformant        : constant Int := 16#3042#;
   Config_OpenGL_Bit        : constant Int := 16#0008#;
   Config_Renderable_Type   : constant Int := 16#3040#;
   Config_Surface_Type      : constant Int := 16#3033#;
   Config_Window_Bit        : constant Int := 16#0004#;
   Config_Min_Swap_Interval : constant Int := 16#303B#;

   Config_Red     : constant Int := 16#3024#;
   Config_Green   : constant Int := 16#3023#;
   Config_Blue    : constant Int := 16#3022#;
   Config_Alpha   : constant Int := 16#3021#;
   Config_Depth   : constant Int := 16#3025#;
   Config_Stencil : constant Int := 16#3026#;
   Config_Samples : constant Int := 16#3031#;

   function Get_Value
     (Object    : Config;
      Display   : Displays.Display;
      Attribute : Int) return Natural
   is
      Value : Int;
   begin
      if not Boolean (API.Get_Config_Attrib (Display.ID, Object.ID, Attribute, Value)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;

      return Natural (Value);
   end Get_Value;

   function State (Object : Config) return Config_State is (Object.State);

   function Get_Configs
     (Display                 : Displays.Display;
      Red, Green, Blue, Alpha : Natural;
      Depth, Stencil          : Natural;
      Samples                 : Sample_Size) return Config_Array
   is
      Attributes : constant Int_Array :=
        (Config_Caveat, None,
         Config_Conformant, Config_OpenGL_Bit,
         Config_Renderable_Type, Config_OpenGL_Bit,
         Config_Color_Buffer_Type, Config_RGB_Buffer,
         Config_Surface_Type, Config_Window_Bit,
         Config_Red, Int (Red),
         Config_Green, Int (Green),
         Config_Blue, Int (Blue),
         Config_Alpha, Int (Alpha),
         Config_Depth, Int (Depth),
         Config_Stencil, Int (Stencil),
         Config_Samples, Int (Samples),
         Config_Min_Swap_Interval, 0,
         None);

      Max_Configs : constant := 32;

      IDs   : ID_Array (1 .. Max_Configs);
      Count : Int := 0;
   begin
      if not Boolean (API.Choose_Config (Display.ID, Attributes, IDs, IDs'Length, Count)) then
         Errors.Raise_Exception_On_EGL_Error;
      end if;

      return Result : Configs.Config_Array (1 .. Natural (Count)) do
         for Index in Result'Range loop
            Result (Index).Reference.ID := IDs (Index);
            Result (Index).State :=
              (Red     => Get_Value (Result (Index), Display, Config_Red),
               Green   => Get_Value (Result (Index), Display, Config_Green),
               Blue    => Get_Value (Result (Index), Display, Config_Blue),
               Alpha   => Get_Value (Result (Index), Display, Config_Alpha),
               Depth   => Get_Value (Result (Index), Display, Config_Depth),
               Stencil => Get_Value (Result (Index), Display, Config_Stencil),
               Samples => Sample_Size (Get_Value (Result (Index), Display, Config_Samples)));
         end loop;
      end return;
   end Get_Configs;

end EGL.Objects.Configs;
