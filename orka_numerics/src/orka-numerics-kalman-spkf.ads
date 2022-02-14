--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2022 onox <denkpadje@gmail.com>
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

generic

   with function Points_Covariance (Covariance : Matrix) return Matrix;

   with function Transform
     (Phase         : Update_Phase;
      Points, Noise : Matrix;
      Weights       : Weights_Type) return State_Covariance;

   with function Cross_Covariance
     (X, Y             : Matrix;
      State_X, State_Y : State_Covariance;
      Weights          : Weights_Type) return Matrix;

   with function Kalman_Gain
     (State_Y          : State_Covariance;
      Cross_Covariance : Matrix) return Matrix;

   with function Posterior_Covariance
     (State_X, State_Y : State_Covariance;
      Kalman_Gain      : Matrix) return Matrix;

package Orka.Numerics.Kalman.SPKF is
   pragma Preelaborate;

   --  This package implements the sigma-point Kalman filters from [1]
   --
   --  [1] "Sigma-Point Kalman Filters for Probabilistic Inference in
   --      Dynamic State-Space Models", van der Merwe R.,
   --      Oregon Health & Science University, 2004

   procedure Predict_Update
     (Object      : in out Filter;
      F           : not null access function (Point : Vector; DT : Orka.Float_64) return Vector;
      H           : not null access function (Point : Vector) return Vector;
      DT          : Duration;
      Measurement : Vector);

end Orka.Numerics.Kalman.SPKF;
