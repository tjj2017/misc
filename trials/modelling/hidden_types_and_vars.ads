package Hidden_Types_And_Vars is
   type My_Int is range -1024 .. 1023;
   subtype My_Sub_Int is My_Int range 0 .. 10;

   subtype Int is Integer;
   subtype Sub_Int is Int range 23 ..29;

   Var_Int : Int;
   Var_My_Int : My_Int;

   Var_Sub_Int : Sub_Int;
   Var_My_Sub_Int : My_Sub_Int;
end Hidden_Types_And_Vars;
