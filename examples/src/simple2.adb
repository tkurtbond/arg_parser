with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Arg_Parser;      use Arg_Parser;
with Simple2_Args; use Simple2_Args;

procedure Simple2 is
   procedure Tab is
   begin
      Set_Col (35);
   end Tab;

begin
   Put_Line ("This is output from parsing the command line in the program SIMPLE2.");
   New_Line;

   Parse_Arguments (AP);
   New_Line;
   Put_Line ("After parsing the arguments:");
   New_Line;

   Put ("The_String: "); Tab;
   Put_Line (if The_String /= null then The_String.all else "has not been set!");

   Put ("The_Unbounded_String is "); Tab;
   Put_Line ("""" & (+The_Unbounded_String) & """.");

   Put ("The_Integer (Default " & Trim (The_Integer_Default'Image, Both) & ") is "); Tab;
   Put_Line (Trim (The_Integer'Image, Both));

   Put ("The_Natural (Default " & Trim (The_Natural_Default'Image, Both) & ") is "); Tab;
   Put_Line (Trim (The_Natural'Image, Both));

   Put ("The_Positive (Default " & Trim (The_Positive_Default'Image, Both) & ") is "); Tab;
   Put_Line (Trim (The_Positive'Image, Both));

   Put ("The_True (Default " & The_True_Default'Image & ") is "); Tab;
   Put_Line (The_True'Image);

   Put ("The_False (Default " & The_False_Default'Image & ") is "); Tab;
   Put_Line (Trim (The_False'Image, Both));

   Put ("The_Toggle (Default " & The_Toggle_Default'Image & ") is "); Tab;
   Put_Line (Trim (The_Toggle'Image, Both));
end Simple2;
