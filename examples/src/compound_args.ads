with Arg_Parser; use Arg_Parser;

package Compound_Args is

   Unknown_Command : exception;

   function Do_Main_Help return Boolean;
   function Do_Main_Verbose return Boolean;
   function Do_Main_Level (Arg : Integer) return Boolean;
   function Main_Argument_Handler (Start_With : Positive; Arg : String) return Boolean;

   Main_Options : aliased Option_Array :=
     (Make_Option ("Display the main help.", 'h', "help", Do_Main_Help'Access),
      Make_Option ("Set main verbosity on.", 'v', "verbose", Do_Main_Verbose'Access),
      Make_Integer_Option ("Set the level to ARG.", 'l', "level", Do_Main_Level'Access));

   function Do_List_Help return Boolean;
   --  You could have a separate verbose flag for the list command.
   function Do_List_Verbose return Boolean;
   function Do_List_Regexp (Arg : String) return Boolean;
   function List_Argument_Handler (Start_With : Positive; Arg : String) return Boolean;

   List_Options : aliased Option_Array :=
     (Make_Option ("Display the help for the list command.", 'h', "help", Do_List_Help'Access),
      Make_Option ("Set the list command verbosity on.", 'v', "verbose", Do_List_Verbose'Access),
      Make_String_Option ("Set the regexp to be used to ARG.", 'r', "regexp", Do_List_Regexp'Access));

   function Do_Show_Details return Boolean;
   function Do_Show_Parsable return Boolean;
   function Do_Show_Format (Arg : String) return Boolean;
   function Show_Argument_Handler (Start_With : Positive; Arg : String) return Boolean;

   Show_Options : aliased Option_Array :=
     (Make_Option ("Output the details of the item.", 'd', "details", Do_Show_Details'Access),
      Make_Option ("Display the output in a parsable form.", 'p', "parsable", Do_Show_Parsable'Access),
      Make_String_Option ("Use ARG for the output format.", 'f', "format", Do_Show_Format'Access));

   Commands : aliased Command_Array :=
     (Make_Command
        ("list",
         Make_Argument_Parser
           (Description => "list command", Handler => List_Argument_Handler'Access, Options => List_Options'Access)),
      Make_Command
        ("show",
         Make_Argument_Parser
           (Description => "Show command", Handler => Show_Argument_Handler'Access, Options => Show_Options'Access)));

   AP : Argument_Parser :=
     Make_Argument_Parser
       ("Usage: compund [options] [args] " & ASCII.LF & ASCII.LF &
        "Example of a program that implements multiple commands.",
        Main_Argument_Handler'Access,
        Main_Options'Access,
        Commands'Access);

end Compound_Args;
