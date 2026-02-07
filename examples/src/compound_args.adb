with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Compound_Args is

   function Do_Main_Help return Boolean is
   begin
      Usage (AP);
      --  Once they ask for help it is too late to continue.
      return False;
   end Do_Main_Help;

   function Do_Main_Verbose return Boolean is
   begin
      Put_Line ("Do_Main_Verbose called");
      return True;
   end Do_Main_Verbose;

   function Do_Main_Level (Arg : Integer) return Boolean is
   begin
      Put_Line ("Do_Main_Level called with """ & Trim (Arg'Image, Both) & """");
      return True;
   end Do_Main_Level;

   function Main_Argument_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Main_Argument_Handler called with """ & Arg & """");
      return False;
   end Main_Argument_Handler;

   function Do_List_Help return Boolean is
   begin
      Usage (AP);       -- Always use the top level argument parser!
      --  Once they ask for help it is too late to continue.
      return False;
   end Do_List_Help;

   function Do_List_Verbose return Boolean is
   begin
      Put_Line ("Do_List_Verbose called");
      return True;
   end Do_List_Verbose;

   function Do_List_Regexp (Arg : String) return Boolean is
   begin
      Put_Line ("Do_List_Regexp called with """ & Arg & """");
      return True;
   end Do_List_Regexp;

   function List_Argument_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("List_Argument_Handler called with """ & Arg & """");
      return True;
   end List_Argument_Handler;

   function Do_Show_Details return Boolean is
   begin
      Put_Line ("Do_Show_Details called");
      return True;
   end Do_Show_Details;

   function Do_Show_Parsable return Boolean is
   begin
      Put_Line ("Do_Show_Parsable called");
      return True;
   end Do_Show_Parsable;

   function Do_Show_Format (Arg : String) return Boolean is
   begin
      Put_Line ("Do_Show_Format called with """ & Arg & """");
      return True;
   end Do_Show_Format;

   function Show_Argument_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Show_Argument_Handler called with """ & Arg & """");
      return True;
   end Show_Argument_Handler;

end Compound_Args;
