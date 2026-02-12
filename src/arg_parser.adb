with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;

package body Arg_Parser is

   pragma Assertion_Policy (CHECK);

   function "+"(S : String) return Unbounded_String
   renames To_Unbounded_String;

   Out_File : File_Access := Standard_Output;

   procedure Error_Seen is
   begin
      Out_File := Standard_Error;
   end Error_Seen;

   procedure UFlush is
   begin
      Flush (Out_File.all);
   end UFlush;

   procedure UPut (Message : String) is
   begin
      Put (Out_File.all, Message);
      UFlush;
   end UPut;

   procedure UPut_Line (Message : String) is
   begin
      Put_Line (Out_File.all, Message);
      UFlush;
   end UPut_Line;

   procedure UNew_Line is
   begin
      New_Line (Out_File.all);
      UFlush;
   end UNew_Line;

   procedure USet_Col (To : Positive_Count) is
   begin
      Set_Col (Out_File.all, To);
   end USet_Col;

   function Make_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Option_Handler) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (No_Argument_Option, Short_Name, +Long_Name, False,
               +Description, Handler);
   end Make_Option;

   function Make_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null String_Option_Handler) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => String_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              String_Handler    => Handler);
   end Make_String_Option;

   function Make_Set_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access String_Reference) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => Set_String_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              String_Ref        => Variable);
   end Make_Set_String_Option;

   function Make_Unbounded_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Unbounded_String_Option_Handler) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind                     => Unbounded_String_Option,
              Short_Name               => Short_Name,
              Long_Name                => +Long_Name,
              Argument_Required        => True,
              Description              => +Description,
              Unbounded_String_Handler => Handler);
   end Make_Unbounded_String_Option;

   function Make_Set_Unbounded_String_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Unbounded_String) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind                 => Set_Unbounded_String_Option,
              Short_Name           => Short_Name,
              Long_Name            => +Long_Name,
              Argument_Required    => True,
              Description          => +Description,
              Unbounded_String_Ref => Variable);
   end Make_Set_Unbounded_String_Option;

   function Make_Integer_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Integer_Option_Handler;
      First        : Integer := Integer'First;
      Last         : Integer := Integer'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind               => Integer_Option,
              Short_Name         => Short_Name,
              Long_Name          => +Long_Name,
              Argument_Required  => True,
              Description        => +Description,
              Integer_Handler    => Handler,
              Integer_First      => First,
              Integer_Last       => Last);
   end Make_Integer_Option;

   function Make_Set_Integer_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Integer;
      First        : Integer := Integer'First;
      Last         : Integer := Integer'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => Set_Integer_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              Integer_Ref       => Variable,
              Set_Integer_First => First,
              Set_Integer_Last  => Last);
   end Make_Set_Integer_Option;

   function Make_Natural_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Natural_Option_Handler;
      First        : Natural := Natural'First;
      Last         : Natural := Natural'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => Natural_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              Natural_Handler   => Handler,
              Natural_First     => First,
              Natural_Last      => Last);
   end Make_Natural_Option;

   function Make_Set_Natural_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Natural;
      First        : Natural := Natural'First;
      Last         : Natural := Natural'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => Set_Natural_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              Natural_Ref       => Variable,
              Set_Natural_First => First,
              Set_Natural_Last  => Last);
   end Make_Set_Natural_Option;

   function Make_Positive_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Handler      : not null Positive_Option_Handler;
      First        : Positive := Positive'First;
      Last         : Positive := Positive'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind              => Positive_Option,
              Short_Name        => Short_Name,
              Long_Name         => +Long_Name,
              Argument_Required => True,
              Description       => +Description,
              Positive_Handler  => Handler,
              Positive_First    => First,
              Positive_Last     => Last);
   end Make_Positive_Option;

   function Make_Set_Positive_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Positive;
      First        : Positive := Positive'First;
      Last         : Positive := Positive'Last) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind               => Set_Positive_Option,
              Short_Name         => Short_Name,
              Long_Name          => +Long_Name,
              Argument_Required  => True,
              Description        => +Description,
              Positive_Ref       => Variable,
              Set_Positive_First => First,
              Set_Positive_Last  => Last);
   end Make_Set_Positive_Option;

   ------------------------------------------------------------------------------------------
   --  The {True,False,Toggle} boolean options never take arguments.
   ------------------------------------------------------------------------------------------

   function Make_Set_Boolean_True_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind => Set_Boolean_True_Option,
              Short_Name           => Short_Name,
              Long_Name            => +Long_Name,
              Argument_Required    => False,
              Description          => +Description,
              Set_Boolean_True_Ref => Variable);
   end Make_Set_Boolean_True_Option;

   function Make_Set_Boolean_False_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind => Set_Boolean_False_Option,
              Short_Name           => Short_Name,
              Long_Name            => +Long_Name,
              Argument_Required    => False,
              Description          => +Description,
              Set_Boolean_False_Ref => Variable);
   end Make_Set_Boolean_False_Option;

   function Make_Toggle_Boolean_Option
     (Description  : String    := "";
      Short_Name   : Character := ASCII.NUL;
      Long_Name    : String    := "";
      Variable     : not null access Boolean) return Option is
   begin
      pragma Assert
        ((Short_Name /= ASCII.NUL) or (Long_Name'Length > 0),
         "Neither a Short Name or a Long Name were supplied for option with the description """ & Description & """");
      return (Kind => Toggle_Boolean_Option,
              Short_Name           => Short_Name,
              Long_Name            => +Long_Name,
              Argument_Required    => False,
              Description          => +Description,
              Toggle_Boolean_Ref   => Variable);
   end Make_Toggle_Boolean_Option;

   function Make_Command
     (Command_Name : String;
      Arg_Parser   : Argument_Parser) return Command is
   begin
      return (+Command_Name, Arg_Parser);
   end Make_Command;

   function Make_Argument_Parser
     (Description : String;
      Handler     : not null Argument_Handler;
      Options     : Option_Array_Access) return Argument_Parser is
   begin
      return (+Description, Handler, Options, null);
   end Make_Argument_Parser;

   function Make_Argument_Parser
     (Description : String;
      Handler     : not null Argument_Handler;
      Options     : Option_Array_Access;
      Commands    : not null Command_Array_Access) return Argument_Parser is
   begin
      return (+Description, Handler, Options, Commands);
   end Make_Argument_Parser;

   procedure Usage (Arg_Parser : Argument_Parser) is
      procedure Format_Option (The_Option : Option) is
         Arg_Width       : constant Positive_Count := 30;
         Both_Names      : Boolean := The_Option.Short_Name /= ASCII.NUL and The_Option.Long_Name /= Null_Unbounded_String;
         Short_Name      : String                  :=
           (if The_Option.Short_Name = ASCII.NUL then ""
            else '-' & The_Option.Short_Name & (if The_Option.Argument_Required then " ARG" else ""));
         Long_Name       : String                  :=
           (if The_Option.Long_Name = Null_Unbounded_String then ""
            else "--" & To_String (The_Option.Long_Name) & (if The_Option.Argument_Required then "=ARG" else ""));
         Arg_Description : String                  := Short_Name & (if Both_Names then ", " else "") & Long_Name;
         Break_Line      : Boolean                 := Arg_Description'Length > Arg_Width - 1;
      begin
         UPut (Arg_Description);
         if The_Option.Description /= Null_Unbounded_String then
            if Break_Line then
               UNew_Line;  -- Leave at least one space.
            else
               USet_Col (Arg_Width + 1);
            end if;
            UPut (To_String (The_Option.Description));
         end if;
         UNew_Line;
      end Format_Option;
   begin
      if Arg_Parser.Description /= Null_Unbounded_String then
         UPut_Line (To_String (Arg_Parser.Description));
      end if;
      UNew_Line;
      if Arg_Parser.Options /= null then
         for The_Option of Arg_Parser.Options.all loop
            Format_Option (The_Option);
         end loop;
      end if;
      if Arg_Parser.Commands /= null then
         for Command of Arg_Parser.Commands.all loop
            UNew_Line;
            Usage (Command.Parser);
         end loop;
      end if;
   end Usage;

   procedure Parse_Arguments
     (Arg_Parser : Argument_Parser;
      Start_With : Positive := 1)
   is
      Options  : Option_Array_Access renames Arg_Parser.Options;
      Commands : Command_Array_Access renames Arg_Parser.Commands;

      function Get_Option (Prefix : String; Arg : String) return Positive is
         Unbounded_Arg : Unbounded_String := +Arg;
      begin
         if Options /= null then
            for I in Options'Range loop
               if Unbounded_Arg = Options (I).Long_Name
                  or else (Arg'Length = 1 and then Arg (Arg'First) /= ASCII.NUL
                           and then Arg (Arg'First) = Options (I).Short_Name)
               then
                  return I;
               end if;
            end loop;
         end if;
         --  If we get here it is an unknown option.
         Error_Seen;
         raise Unknown_Option with Prefix & Arg;
      end Get_Option;

      function Get_Command (Arg : String) return Natural is
         Unbounded_Arg : Unbounded_String := +Arg;
      begin
         if Commands /= null then
            for I in Commands'Range loop
               if Unbounded_Arg = Commands (I).Command_Name then
                  return I;
               end if;
            end loop;
         end if;
         return 0;
      end Get_Command;

      Number_Of_Arguments : constant Natural := Argument_Count;

      Found_Dash_Dash  : Boolean := False;
      Skip_Next_Option : Boolean := False;
      Continue         : Boolean := True;

      --  This handles a command line argument that is not an option.
      procedure Handle_Argument (Arg : String; Arg_Index : Integer) is
         --  Zero indicates no command found.
         Command_Index : Natural := Get_Command (Arg);
      begin
         if Command_Index = 0 then -- It is NOT a command.
            Continue := Arg_Parser.Handler (Arg_Index + 1, Arg);
         else                   -- It IS a command.
            Parse_Arguments (Commands (Command_Index).Parser, Arg_Index + 1);
            --  Never continue once a subcommand returns.
            Continue := False;
         end if;
      end Handle_Argument;

      function Dispatch_Option_Handler (Flag : String; Opt : Option; Arg : String) return Boolean is
         function Verify_Integer (First : Integer; Last : Integer) return Integer is
            Integer_Arg : Integer := Integer'Value (Arg);
         begin
            if Integer_Arg < First or else Integer_Arg > Last then
               raise Invalid_Option_Argument with
                 "Value """ & Arg & """ not in range " &
                 Trim (First'Image, Both) & ".." & Trim (Last'Image, Both) &
                 " for option """ & Flag & """";
            end if;
            return Integer_Arg;
         exception
            when Constraint_Error =>
               raise Invalid_Option_Argument with "Invalid option argument """ & Arg & """ for option """ & Flag & """";
         end Verify_Integer;

      begin
         --  Natural_Option, Set_Natural_Option, Positive_Option,
         --  Set_Positive_Option all just use Verify_Integer, because
         --  the *_First and *_Last values are constrainted by the
         --  subtype on the arguments to the Make_* functions and by
         --  subtypes in the Option type.
         case Opt.Kind is
            when No_Argument_Option =>
               return Opt.Handler.all;
            when String_Option =>
               return Opt.String_Handler (Arg);
            when Set_String_Option =>
               Opt.String_Ref.all := new String'(Arg);
               return True;
            when Unbounded_String_Option =>
               return Opt.Unbounded_String_Handler (+Arg);
            when Set_Unbounded_String_Option =>
               Opt.Unbounded_String_Ref.all := +Arg;
               return True;
            when Integer_Option =>
               return Opt.Integer_Handler (Verify_Integer (Opt.Integer_First, Opt.Integer_Last));
            when Set_Integer_Option =>
               Opt.Integer_Ref.all := Verify_Integer (Opt.Set_Integer_First, Opt.Set_Integer_Last);
               return True;
            when Natural_Option =>
               return Opt.Natural_Handler (Verify_Integer (Opt.Natural_First, Opt.Natural_Last));
            when Set_Natural_Option =>
               Opt.Natural_Ref.all :=  Verify_Integer (Opt.Set_Natural_First, Opt.Set_Natural_Last);
               return True;
            when Positive_Option =>
               return Opt.Positive_Handler (Verify_Integer (Opt.Positive_First, Opt.Positive_Last));
            when Set_Positive_Option =>
               Opt.Positive_Ref.all := Verify_Integer (Opt.Set_Positive_First, Opt.Set_Positive_Last);
               return True;
            when Set_Boolean_True_Option =>
               Opt.Set_Boolean_True_Ref.all := True;
               return True;
            when Set_Boolean_False_Option =>
               Opt.Set_Boolean_False_Ref.all := False;
               return True;
            when Toggle_Boolean_Option =>
               Opt.Toggle_Boolean_Ref.all := not Opt.Toggle_Boolean_Ref.all;
               return True;
         end case;
      end Dispatch_Option_Handler;

      procedure Handle_Long_Option (Arg : String; Arg_Index : Integer) is
         Equals_Index : Natural := Index (Arg, "=");
         Option_Name  : String  :=
           (if Equals_Index > 0 then
               Arg (Arg'First + 2 .. Equals_Index - 1)
            else
               Arg (Arg'First + 2 .. Arg'Last));
         Option_Index : Natural := Get_Option ("--", Option_Name);

         function Find_Option_Argument return String is
         begin
            if Equals_Index > 0 and then Arg'Last >= Equals_Index + 1 then
               return Arg (Equals_Index + 1 .. Arg'Last);
            elsif Arg_Index + 1 <= Number_Of_Arguments then
               Skip_Next_Option := True;
               return Argument (Arg_Index + 1);
            else
               Error_Seen;
               raise Argument_Required with Arg;
            end if;
         end Find_Option_Argument;
      begin
         if Option_Index > 0 then
            declare
               Opt      : Option renames Options (Option_Index);
               Option_Argument : String :=
                 (if Opt.Argument_Required then Find_Option_Argument else "");
            begin
               Continue := Dispatch_Option_Handler ("--" & Option_Name, Opt, Option_Argument);
            end;
         end if;
      end Handle_Long_Option;

      procedure Handle_Short_Options (Arg : String; Arg_Index : Integer) is
      begin
         for J in 2 .. Arg'Last loop
            declare
               Option_Name  : String  := Arg (J .. J);
               Option_Index : Natural := Get_Option ("-", Option_Name);
            begin
               if Option_Index > 0 then
                  declare
                     Opt : Option renames Options (Option_Index);
                  begin
                     if Opt.Argument_Required then
                        if Arg_Index = Number_Of_Arguments
                           or else (Argument (Arg_Index + 1)'Length > 1
                                    and then Argument (Arg_Index + 1) (1) = '-')
                        then
                           Error_Seen;
                           raise Argument_Required with "-" & Arg (J);
                        end if;
                        Continue := Dispatch_Option_Handler (Option_Name, Opt, Argument (Arg_Index + 1));
                        Skip_Next_Option := True;
                     else
                        Continue := Dispatch_Option_Handler (Option_Name, Opt, "");
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Handle_Short_Options;

   begin
      for Arg_Index in Start_With .. Number_Of_Arguments loop
         exit when not Continue;
         declare
            --  We use Ada.Command_Line.Argument, so we call things "Arg" instead.
            Arg : String := Argument (Arg_Index);
         begin
            if Skip_Next_Option then
               Skip_Next_Option := False;
            elsif Arg = "--" and then not Found_Dash_Dash then
               Found_Dash_Dash := True;
            elsif Found_Dash_Dash or else Arg'Length = 0 or else
                  (Arg'Length >= 1 and then Arg (Arg'First) /= '-')
            then
               Handle_Argument (Arg, Arg_Index);
            elsif Arg'Last > 1 and then Arg (2) = '-' then
               Handle_Long_Option (Arg, Arg_Index);
            else
               Handle_Short_Options (Arg, Arg_Index);
            end if;
         end;
      end loop;
   exception
      when Error : Unknown_Option    =>
         UPut_Line ("Unknown option " & Exception_Message (Error));
         UNew_Line;
         Usage (Arg_Parser);
         raise;
      when Error : Unknown_Argument  =>
         UPut_Line ("Unknown argument " & Exception_Message (Error));
         UNew_Line;
         Usage (Arg_Parser);
         raise;
      when Error : Argument_Required =>
         UPut_Line ("Argument Required for option " & Exception_Message (Error));
         New_Line;
         Usage (Arg_Parser);
         raise;
      when Error : Invalid_Option_Argument =>
         UPut_Line ("Invalid value for option " & Exception_Message (Error));
   end Parse_Arguments;
end Arg_Parser;
