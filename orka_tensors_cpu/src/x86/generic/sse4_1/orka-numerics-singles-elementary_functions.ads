with Orka.SIMD_Emulation_Elementary_Functions;
with Orka.SIMD.SSE.Singles;

package Orka.Numerics.Singles.Elementary_Functions is new Orka.SIMD_Emulation_Elementary_Functions
  (Index_4D,
   Float_32,
   SIMD.SSE.Singles.m128);
pragma Preelaborate (Orka.Numerics.Singles.Elementary_Functions);
