with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Arg_Parser; use Arg_Parser;

procedure Simple is

   function "+" (S : String) return Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (S : Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;

   The_String : aliased String_Reference; -- It is an access to an access to a string, since you can't have an unconstrained String.
   The_Unbounded_String_Default : constant Unbounded_String := +"Default Unbounded_String";
   The_Unbounded_String : aliased Unbounded_String :=  The_Unbounded_String_Default;
   The_Integer_Default : constant Integer := -2;
   The_Integer : aliased Integer := The_Integer_Default;
   The_Natural_Default : constant Natural := 0;
   The_Natural : aliased Natural := The_Natural_Default;
   The_Positive_Default : constant Positive := 100;
   The_Positive : aliased Positive := The_Positive_Default;
   --  Defaults to False so we can set it to True.
   The_True_Default : constant Boolean := False;
   The_True : aliased Boolean := The_True_Default;
   --  Defaults to True so we can set it to False.
   The_False_Default : constant Boolean := True;
   The_False : aliased Boolean := The_False_Default;
   --  Defaults to True so we can toggle it to whatever we want.
   The_Toggle_Default : constant Boolean := True;
   The_Toggle : aliased Boolean := The_Toggle_Default;

   ------------------------------------------------------------------------------------------
   --  NOTE: All the handler functions return True to continue the parse or false to stop it.
   ------------------------------------------------------------------------------------------

   function Do_Help return Boolean;
   --  This is a forward declaration because it needs to call Usage on
   --  the AP, which is declared later.

   function Do_String (Arg : String) return Boolean is
   begin
      Put_Line ("Do_String called with """ & Arg & """.");
      return True;
   end Do_String;

   function Do_Unbounded_String (Arg : Unbounded_String) return Boolean is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line ("Do_Unbounded_String called with """ & Arg & """.");
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

   Options : aliased Option_Array :=
     (Make_Option
       (Description  => "Call a function for an option that has no argument.  Also, print a help message.",
        Short_Name   => 'h',
        Long_Name    => "help",
        Handler      => Do_Help'Unrestricted_Access),
       Make_String_Option
         (Description => "Call a function on the argument of a string option",
          Short_Name  => 's',
          Long_Name   => "string",
          Handler     => Do_String'Unrestricted_Access),
       Make_Set_String_Option
         (Description  => "Set an, via an accesss, a String to the argument of an option.",
          Short_Name => 'S',
          Long_Name => "set-string",
          Variable => The_String'Unrestricted_Access),
       Make_Unbounded_String_Option
         (Description => "Call a function that takes an Unbounded_String on the argument option.",
          Short_Name => 'u',
          Long_Name => "unbounded",
          Handler => Do_Unbounded_String'Unrestricted_Access),
       Make_Set_Unbounded_String_Option
         (Description => "Set an Unbounded_String variable to the argument to an option",
          Short_Name => 'U',
          Long_Name => "set-unbounded",
          Variable => The_Unbounded_String'Unrestricted_Access),
       Make_Integer_Option
         (Description => "Call a function on an Integer that was the value of an argument to an option.",
          Short_Name  => 'i',
          Long_Name   => "integer",
          Handler     => Do_Integer'Unrestricted_Access,
          First       => -100,
          Last        => 100),
       Make_Set_Integer_Option
         (Description => "Set an Integer variable to the value of the argument to an option",
          Short_Name => 'I',
          Long_Name => "set-integer",
          Variable => The_Integer'Unrestricted_Access),
       Make_Natural_Option
         (Description => "Call a function on an Natural that was the value of an argument to an option.",
          Short_Name  => 'n',
          Long_Name   => "natural",
          Handler     => Do_Natural'Unrestricted_Access,
          First       => 0,
          Last        => 100),
       Make_Set_Natural_Option
         (Description => "Set an Natural variable to the value of the argument to an option",
          Short_Name => 'N',
          Long_Name => "set-natural",
          Variable => The_Natural'Unrestricted_Access),
       Make_Positive_Option
         (Description => "Call a function on an Positive that was the value of an argument to an option.",
          Short_Name  => 'p',
          Long_Name   => "positive",
          Handler     => Do_Positive'Unrestricted_Access,
          First       => 1,
          Last        => 100),
       Make_Set_Positive_Option
         (Description => "Set an Positive variable to the value of the argument to an option",
          Short_Name => 'P',
          Long_Name => "set-positive",
          Variable => The_Positive'Unrestricted_Access),
       Make_Set_Boolean_True_Option
         (Description => "Set a Boolean variable to true directly.",
          Short_Name => 't',
          Long_Name => "set-true",
          Variable => The_True'Unrestricted_Access),
       Make_Set_Boolean_False_Option
         (Description => "Set a Boolean variable to false directly.",
          Short_Name => 'f',
          Long_Name => "set-false",
          Variable => The_False'Unrestricted_Access),
       Make_Toggle_Boolean_Option
         (Description => "Toggle a Boolean variable directly.",
          Short_Name => 'T',
          Long_Name => "toggle",
          Variable => The_Toggle'Unrestricted_Access),
       Make_Option
         (Description => "This option has only a short name, and calls a function with no argument.",
          Short_Name  => 'a',
          Handler     => Do_Short_Name_Only'Unrestricted_Access),
       Make_Option
         (Description => "This option has only a long name, and calls a function with no argument.",
          Long_Name   => "long-name",
          Handler     => Do_Long_Name_Only'Unrestricted_Access)
     );

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Arg_Handler called with """ & Arg & """");
      return True;
   end Arg_Handler;

   AP : Argument_Parser :=
     Make_Argument_Parser ("simple [options] arguments...", Arg_Handler'Unrestricted_Access, Options'Unrestricted_Access);

   function Do_Help return Boolean is
      End_Program : exception;
   begin
      Usage (AP);
      --  Once they ask for help it is too late to continue.
      raise End_Program;
      return False;
   end Do_Help;

   procedure Tab is
   begin
      Set_Col (35);
   end Tab;

begin
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
end Simple;
