--  need_either - Show that an option requires either a short name or a long name.

with Ada.Text_IO; use Ada.Text_IO;

with Arg_Parser; use Arg_Parser;

procedure Need_Either is

   function Do_Must_Have_Short_Or_Long_Name return Boolean is
   begin
      return True;
   end Do_Must_Have_Short_Or_Long_Name;

   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean is
   begin
      Put_Line ("Arg_Handler called with """ & Arg & """");
      return True;
   end Arg_Handler;

   --  This option has neither a short name form nor a long name form.
   --  It therefore couldn't be called.  So it causes a assertion
   --  failure at run time.  Since this will show up any time the
   --  program is run, the first time the argument parser is invoked,
   --  the author of the code should be able to find this bug easily.
   Options : aliased Option_Array :=
     (1 =>
        Make_Option
          (Description  => "Either Short_Name or Long_Name has to be specified and non-null.",
           Handler      => Do_Must_Have_Short_Or_Long_Name'Unrestricted_Access));

   AP : Argument_Parser :=
     Make_Argument_Parser ("simple [options] arguments...", Arg_Handler'Unrestricted_Access, Options'Unrestricted_Access);

begin
   Parse_Arguments (AP, 1);
end Need_Either;
