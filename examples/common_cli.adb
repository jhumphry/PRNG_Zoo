--
-- PRNG Zoo
-- Copyright 2014 - 2015 James Humphry
--

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Containers;
use type Ada.Containers.Count_Type;

with Parse_Args;
with PRNG_Zoo.Register;
use PRNG_Zoo.Register;

procedure Common_CLI(AP : in out Parse_Args.Argument_Parser;
                     PRNG_Names : in out Parse_Args.String_Doubly_Linked_Lists.List) is
   I : Register_Cursor;
begin

   PRNG_Names := Parse_Args.String_Doubly_Linked_Lists.Empty_List;

   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "list-prng", 'p',
                 Usage => "List available PRNG", Prepend_Usage => True);
   AP.Add_Option(Parse_Args.Make_Boolean_Option(False), "help", 'h',
                 Usage => "Display this help text", Prepend_Usage => True);

   AP.Allow_Tail_Arguments;

   AP.Parse_Command_Line;

   if not AP.Parse_Success then
      Put_Line("Error while parsing command-line arguments: " & AP.Parse_Message);
   elsif AP.Boolean_Value("help") then
      AP.Usage;
   elsif AP.Boolean_Value("list-prng") then
      PRNG_Zoo.Register.Display_Register;
   elsif AP.Tail.Length = 0 then
      Put_Line("At least one PRNG must be specified.");
   elsif AP.Tail.First_Element = "all" then
      for I in Register.Iterate loop
         PRNG_Names.Append (PRNG_Registries.Key (I));
      end loop;
   else
      for J of AP.Tail loop
         if not PRNG_Zoo.Register.PRNG_Exists(J) then
            PRNG_Names := Parse_Args.String_Doubly_Linked_Lists.Empty_List;
            Put_Line("No such PRNG: '" & J & "'");
            exit;
         end if;
         PRNG_Names.Append (J);
      end loop;

   end if;

end Common_CLI;
