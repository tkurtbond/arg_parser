with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  Arg_Parser is a command line argument parser.  It calls user provided
--  functions on each command line argument, deciding if they are
--  options or not options and calls the appropriate function.

package Arg_Parser is
   Unknown_Option, Unknown_Argument, Argument_Required, Invalid_Option_Argument : exception;

   type String_Reference is access all String;

   --  Information about a command line option.
   type Option is limited private;
   type Option_Array is array (Positive range <>) of Option;
   type Option_Array_Access is access all Option_Array;

   type Option_Handler is access function return Boolean;
   --  The type of the function to be called to process an option without an argument.
   --  @return True if the argument parser should continue parsing, False
   --  if it should exit.
   type String_Option_Handler is access function (Arg : String) return Boolean;
   type Unbounded_String_Option_Handler is access function (Arg : Unbounded_String) return Boolean;
   type Integer_Option_Handler is access function (Arg : Integer) return Boolean;
   type Natural_Option_Handler is access function (Arg : Natural) return Boolean;
   type Positive_Option_Handler is access function (Arg : Positive) return Boolean;

   type Argument_Handler is access function (Start_With : Positive; Arg : String) return Boolean;
   --  The type of the function to be called to process an non-option argument.
   --
   --  @param Start_With The position of the next command line
   --  argument.  This is passed by the argument parser in case this
   --  argument is a command, for use when the program has multiple
   --  commands.
   --  @param Arg The value of the command line argument to be processed.
   --  @return True if the argument parser should continue parsing, False
   --  if it should exit.

   function Make_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Option_Handler) return Option;

   function Make_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null String_Option_Handler) return Option;

   function Make_Set_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access String_Reference) return Option;

   function Make_Unbounded_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Unbounded_String_Option_Handler) return Option;

   function Make_Set_Unbounded_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Unbounded_String) return Option;

   function Make_Integer_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Integer_Option_Handler;
      First        : Integer := Integer'First;
      Last         : Integer := Integer'Last) return Option;

   function Make_Set_Integer_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Integer;
      First        : Integer := Integer'First;
      Last         : Integer := Integer'Last) return Option;

   function Make_Natural_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Natural_Option_Handler;
      First        : Natural := Natural'First;
      Last         : Natural := Natural'Last) return Option;

   function Make_Set_Natural_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Natural;
      First        : Natural := Natural'First;
      Last         : Natural := Natural'Last) return Option;

   function Make_Positive_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Positive_Option_Handler;
      First        : Positive := Positive'First;
      Last         : Positive := Positive'Last) return Option;

   function Make_Set_Positive_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Positive;
      First        : Positive := Positive'First;
      Last         : Positive := Positive'Last) return Option;

   function Make_Set_Boolean_True_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option;

   function Make_Set_Boolean_False_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option;

   function Make_Toggle_Boolean_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option;

   type Argument_Parser is limited private;
   type Argument_Parser_Array is array (Positive range <>) of Argument_Parser;

   type Command is limited private;
   type Command_Array is array (Positive range <>) of Command;
   type Command_Array_Access is access all Command_Array;
   function Make_Command (Command_Name : String; Arg_Parser : Argument_Parser) return Command;

   --  Make an argument parser for programs that DO NOT implement multiple commands.
   function Make_Argument_Parser
     (Description : String; Handler : not null Argument_Handler; Options : Option_Array_Access) return Argument_Parser;

   --  Make an argument parser for programs that implement multiple commands.
   --  It has both a list of options and a list of commands.
   function Make_Argument_Parser
     (Description : String;
      Handler     : not null Argument_Handler;
      Options     : Option_Array_Access;
      Commands    : not null Command_Array_Access) return Argument_Parser;

   --  Print a help message.
   procedure Usage (Arg_Parser : Argument_Parser);

   --  Parse command line arguments.
   procedure Parse_Arguments (Arg_Parser : Argument_Parser; Start_With : Positive := 1);

private

   type Option_Kind is (No_Argument_Option, String_Option, Set_String_Option, Unbounded_String_Option, Set_Unbounded_String_Option,
                        Integer_Option, Set_Integer_Option, Natural_Option, Set_Natural_Option, Positive_Option,
                        Set_Positive_Option, Set_Boolean_True_Option, Set_Boolean_False_Option, Toggle_Boolean_Option);
   type Option (Kind : Option_Kind := No_Argument_Option) is record
      Short_Name        : Character := ASCII.NUL;
      Long_Name         : Unbounded_String;
      Argument_Required : Boolean;
      Description       : Unbounded_String;
      case Kind is
         when No_Argument_Option          =>
            Handler                       : not null Option_Handler;
         when String_Option               =>
            String_Handler                : not null String_Option_Handler;
         when Set_String_Option           =>
            String_Ref                    : not null access String_Reference;
         when Unbounded_String_Option     =>
            Unbounded_String_Handler      : not null Unbounded_String_Option_Handler;
         when Set_Unbounded_String_Option =>
            Unbounded_String_Ref          : not null access Unbounded_String;
         when Integer_Option              =>
            Integer_Handler               : not null Integer_Option_Handler;
            Integer_First                 : Integer;
            Integer_Last                  : Integer;
         when Set_Integer_Option          =>
            Integer_Ref                   : not null access Integer;
            Set_Integer_First             : Integer;
            Set_Integer_Last              : Integer;
         when Natural_Option              =>
            Natural_Handler               : not null Natural_Option_Handler;
            Natural_First                 : Natural;
            Natural_Last                  : Natural;
         when Set_Natural_Option          =>
            Natural_Ref                   : not null access Natural;
            Set_Natural_First             : Natural;
            Set_Natural_Last              : Natural;
         when Positive_Option             =>
            Positive_Handler              : not null Positive_Option_Handler;
            Positive_First                : Positive;
            Positive_Last                 : Positive;
         when Set_Positive_Option         =>
            Positive_Ref                  : not null access Positive;
            Set_Positive_First            : Positive;
            Set_Positive_Last             : Positive;
         when Set_Boolean_True_Option     =>
            Set_Boolean_True_Ref          : not null access Boolean;
         when Set_Boolean_False_Option    =>
            Set_Boolean_False_Ref         : not null access Boolean;
         when Toggle_Boolean_Option       =>
            Toggle_Boolean_Ref            : not null access Boolean;
      end case;
   end record;

   type Command is record
      Command_Name : Unbounded_String;
      --  The description is in the argument parser.
      Parser       : Argument_Parser;
   end record;

   type Argument_Parser is record
      Description : Unbounded_String;
      Handler     : Argument_Handler;
      Options     : Option_Array_Access;
      --  Empty if no commands were defined.
      Commands    : Command_Array_Access;
   end record;

end Arg_Parser;
