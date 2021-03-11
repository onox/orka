with GL.Low_Level.Enums;

with Orka.Contexts.EGL.AWT;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Uniforms;

with AWT.Windows;

package Package_Test is

   package LE renames GL.Low_Level.Enums;

   type Test_Window is limited new Orka.Contexts.EGL.AWT.AWT_Window with record
      FB      : Orka.Rendering.Framebuffers.Framebuffer (Default => True);
      Program : Orka.Rendering.Programs.Program;
      Cursor  : Orka.Rendering.Programs.Uniforms.Uniform (LE.Single_Vec2);
      Resize  : Boolean := False with Atomic;
   end record;

   overriding
   function On_Close (Object : in out Test_Window) return Boolean;

   overriding
   procedure On_Configure
     (Object       : in out Test_Window;
      State        : Standard.AWT.Windows.Window_State);

   overriding
   function Create_Window
     (Context            : Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return Test_Window;

   procedure Post_Initialize (Object : in out Test_Window);

   procedure Render (Object : in out Test_Window);

end Package_Test;
