with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Simple2_Args is

   function Do_Help return Boolean is
   begin
      Usage (AP);
      --  Once they ask for help it is too late to continue.
      return False;
   end Do_Help;

   function Do_Length (Arg : Natural) return Boolean is
   begin
      Put_Line ("Do_Length called with """ & Trim (Arg'Image, Both) & """");
      return True;
   end Do_Length;

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Arg_Handler called with """ & Arg & """");
      return True;
   end Arg_Handler;

end Simple2_Args;
