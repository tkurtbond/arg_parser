with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package body Simple2_Args is

   function Do_String (Arg : String) return Boolean is
   begin
      Put_Line ("Do_String called with """ & Arg & """.");
      return True;
   end Do_String;

   function Do_Unbounded_String (Arg : Unbounded_String) return Boolean is
   begin
      Put_Line ("Do_Unbounded_String called with """ & Arg & """.");
      return True;
   end Do_Unbounded_String;

   function Do_Integer (Arg : Integer) return Boolean is
   begin
      Put_Line ("Do_Integer called with Integer " & Trim (Arg'Image, Both) & ".");
      return True;
   end Do_Integer;

   function Do_Natural (Arg : Natural) return Boolean is
   begin
      Put_Line ("Do_Natural called with Natural " & Trim (Arg'Image, Both) & ".");
      return True;
   end Do_Natural;

   function Do_Positive (Arg : Positive) return Boolean is
   begin
      Put_Line ("Do_Positive called with Positive " & Trim (Arg'Image, Both) & ".");
      return True;
   end Do_Positive;

   ------------------------------------------------------------------------------------------
   --  These show that you can options that have only a Short_Name or only a Long_Name.
   ------------------------------------------------------------------------------------------

   function Do_Short_Name_Only return Boolean is
   begin
      Put_Line ("Do_Short_Name_Only called.");
      return True;
   end Do_Short_Name_Only;

   function Do_Long_Name_Only return Boolean is
   begin
      Put_Line ("Do_Long_Name_Only called.");
      return True;
   end Do_Long_Name_Only;

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Arg_Handler called with Starts_With " & Positive'Image (Start_With) & " and Arg " & Arg & """");
      return True;
   end Arg_Handler;

   function Do_Help return Boolean is
      End_Program : exception;
   begin
      Usage (AP);
      --  Once they ask for help it is too late to continue.
      raise End_Program;
      return False;
   end Do_Help;

end Simple2_Args;
