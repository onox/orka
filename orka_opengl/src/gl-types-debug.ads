--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with GL.Low_Level;

package GL.Types.Debug is
   pragma Preelaborate;

   type Identifier is
     (Texture, Vertex_Array, Buffer, Shader,
      Program, Query, Program_Pipeline, Sampler,
      Framebuffer);

private

   for Identifier use
     (Texture            => 16#1702#,
      Vertex_Array       => 16#8074#,
      Buffer             => 16#82E0#,
      Shader             => 16#82E1#,
      Program            => 16#82E2#,
      Query              => 16#82E3#,
      Program_Pipeline   => 16#82E4#,
      Sampler            => 16#82E6#,
      Framebuffer        => 16#8D40#);
   for Identifier'Size use Low_Level.Enum'Size;

end GL.Types.Debug;
