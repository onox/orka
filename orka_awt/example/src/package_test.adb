with GL.Types;

with Orka.Logging.Default;
with Orka.Resources.Locations.Directories;
with Orka.Rendering.Programs.Modules;
with Orka.Rendering.Drawing;

with AWT.Windows;

package body Package_Test is

   use all type Orka.Logging.Default_Module;
   use all type Orka.Logging.Severity;
   use Orka.Logging;

   procedure Log is new Orka.Logging.Default.Generic_Log (Window_System);

   overriding
   function On_Close (Object : Test_Window) return Boolean is
   begin
      Log (Debug, "User is trying to close window");
      return True;
   end On_Close;

   overriding
   procedure On_Drag
     (Object : in out Test_Window;
      X, Y   : AWT.Inputs.Fixed)
   is
      use all type AWT.Inputs.Action_Kind;
      use type AWT.Inputs.Fixed;
   begin
      Log (Debug, "User dragged something to " &
        "(" & Trim (X'Image) & ", " & Trim (Y'Image) & ")");
      AWT.Drag_And_Drop.Set_Action (if X < 300.0 and Y < 300.0 then Copy else None);
   end On_Drag;

   overriding
   procedure On_Drop
     (Object : in out Test_Window)
   is
      use all type AWT.Inputs.Action_Kind;

      Action : constant AWT.Inputs.Action_Kind := AWT.Drag_And_Drop.Valid_Action;
   begin
      Log (Info, "User dropped something. Action is " & Action'Image);

      if Action /= None then
         Object.Drag_And_Drop_Signal.Set;
      end if;
   end On_Drop;

   procedure Initialize_Framebuffer (Object : in out Test_Window) is
      Alpha : constant Orka.Float_32 := (if Object.State.Transparent then 0.5 else 1.0);
   begin
      Object.FB :=
        Orka.Rendering.Framebuffers.Create_Default_Framebuffer (Object.Width, Object.Height);
      Object.FB.Set_Default_Values ((Color => (0.0, 0.0, 0.0, Alpha), others => <>));
      Object.FB.Use_Framebuffer;
      Log (Debug, "Changed size of framebuffer to " &
        Trim (Object.Width'Image) & " × " & Trim (Object.Height'Image));
   end Initialize_Framebuffer;

   procedure Post_Initialize (Object : in out Test_Window) is
      Location_Shaders : constant Orka.Resources.Locations.Location_Ptr :=
        Orka.Resources.Locations.Directories.Create_Location ("data");
   begin
      Object.Initialize_Framebuffer;

      Object.Program  := Orka.Rendering.Programs.Create_Program
        (Orka.Rendering.Programs.Modules.Create_Module
           (Location_Shaders,
            VS => "cursor.vert",
            FS => "cursor.frag"));
      Object.Program.Use_Program;

      Object.Cursor := Object.Program.Uniform ("cursor");
   end Post_Initialize;

   procedure Render (Object : in out Test_Window) is
      use type Orka.Float_32;
      use AWT.Inputs;

      Window_State  : constant AWT.Windows.Window_State := Object.State;
      Pointer_State : constant AWT.Inputs.Pointer_State := Object.State;

      subtype Float_32 is Orka.Float_32;

      Width  : constant Float_32 := Float_32 (Window_State.Width + 2 * Window_State.Margin);
      Height : constant Float_32 := Float_32 (Window_State.Height + 2 * Window_State.Margin);

      PX : constant Float_32 := Float_32 (Pointer_State.Position (X));
      PY : constant Float_32 := Float_32 (Pointer_State.Position (Y));

      Horizontal : constant Float_32 := PX / Width * 2.0 - 1.0;
      Vertical   : constant Float_32 := PY / Height * 2.0 - 1.0;
   begin
      if Object.Framebuffer_Resized then
         Object.Initialize_Framebuffer;
      end if;

      Object.FB.Clear ((Color => True, others => False));

      Object.Cursor.Set_Vector (Orka.Float_32_Array'(Horizontal, Vertical));
      Orka.Rendering.Drawing.Draw (GL.Types.Points, 0, 1);

      Object.Swap_Buffers;
   end Render;

   overriding
   function Create_Window
     (Context            : aliased Orka.Contexts.Surface_Context'Class;
      Width, Height      : Positive;
      Title              : String  := "";
      Samples            : Natural := 0;
      Visible, Resizable : Boolean := True;
      Transparent        : Boolean := False) return Test_Window is
   begin
      return Result : constant Test_Window :=
        (Orka.Contexts.AWT.Create_Window
          (Context, Width, Height, Title, Samples,
           Visible     => Visible,
           Resizable   => Resizable,
           Transparent => Transparent) with others => <>);
   end Create_Window;

end Package_Test;
