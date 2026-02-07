with Ada.Text_IO;    use Ada.Text_IO;

with Arg_Parser;       use Arg_Parser;
with Compound_Args; use Compound_Args;

procedure Compound is
begin
   Parse_Arguments (AP);
   Put_Line ("Compound is exiting");
end Compound;
