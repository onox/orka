with Orka.SIMD_Emulation_Elementary_Functions;
with Orka.SIMD.SSE2.Doubles;

package Orka.Numerics.Doubles.Elementary_Functions is new Orka.SIMD_Emulation_Elementary_Functions
  (Index_2D,
   Float_64,
   SIMD.SSE2.Doubles.m128d);
pragma Preelaborate (Orka.Numerics.Doubles.Elementary_Functions);
