with GL.Low_Level.Enums;

with Orka.Contexts.AWT;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Programs.Uniforms;

with AWT.Inputs;
with AWT.Windows;

package Package_Test is

   package LE renames GL.Low_Level.Enums;

   type Test_Window is limited new Orka.Contexts.AWT.AWT_Window with record
      FB      : Orka.Rendering.Framebuffers.Framebuffer (Default => True);
      Program : Orka.Rendering.Programs.Program;
      Cursor  : Orka.Rendering.Programs.Uniforms.Uniform (LE.Single_Vec2);
      Resize  : Boolean := False with Atomic;
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

   protected Dnd_Signal is
      procedure Set;

      entry Wait;
   private
      Dropped : Boolean := False;
   end Dnd_Signal;

end Package_Test;
