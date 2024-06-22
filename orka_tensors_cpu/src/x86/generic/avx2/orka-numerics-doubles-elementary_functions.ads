with Orka.SIMD_Emulation_Elementary_Functions;
with Orka.SIMD.AVX.Doubles;

package Orka.Numerics.Doubles.Elementary_Functions is new Orka.SIMD_Emulation_Elementary_Functions
  (Index_4D,
   Float_64,
   SIMD.AVX.Doubles.m256d);
pragma Preelaborate (Orka.Numerics.Doubles.Elementary_Functions);
