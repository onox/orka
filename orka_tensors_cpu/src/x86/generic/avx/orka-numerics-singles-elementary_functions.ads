with Orka.SIMD_Emulation_Elementary_Functions;
with Orka.SIMD.AVX.Singles;

package Orka.Numerics.Singles.Elementary_Functions is new Orka.SIMD_Emulation_Elementary_Functions
  (SIMD.AVX.Index_8D,
   Float_32,
   SIMD.AVX.Singles.m256);
pragma Preelaborate (Orka.Numerics.Singles.Elementary_Functions);
