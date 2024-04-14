with GL.Low_Level.Enums;

with Orka.Contexts.AWT;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Shaders;
with Orka.Rendering.Programs.Uniforms;

with AWT.Inputs;
with AWT.Drag_And_Drop;

package Package_Test is

   package LE renames GL.Low_Level.Enums;

   type Test_Window is limited new Orka.Contexts.AWT.AWT_Window with record
      FB      : Orka.Rendering.Framebuffers.Framebuffer (Default => True);
      Program : Orka.Rendering.Programs.Shaders.Shader_Programs;
      Cursor  : Orka.Rendering.Programs.Uniforms.Uniform (LE.Single_Vec2);

      Drag_And_Drop_Signal : AWT.Drag_And_Drop.Signal;
   end record;

   overriding
   function On_Close (Object : Test_Window) return Boolean;

   overriding
   procedure On_Drag
     (Object : in out Test_Window;
      X, Y   : AWT.Inputs.Fixed);

   overriding
   procedure On_Drop
     (Object : in out Test_Window);

   overriding
   function Create_Window
     (Context            : aliased in out Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return Test_Window;

   procedure Post_Initialize (Object : in out Test_Window);

   procedure Render (Object : in out Test_Window);

end Package_Test;
