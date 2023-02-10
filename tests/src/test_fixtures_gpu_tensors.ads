with AUnit.Test_Fixtures;

with Orka.Contexts.EGL;

package Test_Fixtures_GPU_Tensors is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with record
      Context : Orka.Contexts.EGL.Device_EGL_Context := Orka.Contexts.EGL.Create_Context
        (Version => (4, 2),
         Flags   => (Debug => True, others => False));
   end record;

end Test_Fixtures_GPU_Tensors;
