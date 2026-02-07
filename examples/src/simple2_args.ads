with Arg_Parser; use Arg_Parser;

package Simple2_Args is

   function Do_Help return Boolean;
   function Do_Length (Arg : Natural) return Boolean;
   function Arg_Handler (Start_With : Positive; Arg : String) return Boolean;

   Options : aliased Option_Array :=
     (Make_Option ("Display a help message.", 'h', "help", Do_Help'Access),
      Make_Natural_Option ("Set the width of the output", 'w', "width", Do_Length'Access));

   AP : Argument_Parser :=
     Make_Argument_Parser ("simple2 [options] arguments...", Arg_Handler'Access, Options'Access);

end Simple2_Args;
