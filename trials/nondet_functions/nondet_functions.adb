with Types; use Types;
with Nondet;
with Nondet_In_Type;
procedure Nondet_Functions is
begin
   Var_Sub := 1;
   pragma Assert (Var_Sub = 1);
   pragma Assert (Var_Sub >= My_Sub'First and Var_Sub <= My_Sub'Last);
   pragma Assert (Var_Sub in My_Sub);

   Var_Sub := Nondet;
   pragma Assert (Var_Sub = 1);
   pragma Assert (Var_Sub >= My_Sub'First and Var_Sub <= My_Sub'Last);
   pragma Assert (Var_Sub in My_Sub);

   Var_Sub := 1;
   pragma Assert (Var_Sub = 1);
   pragma Assert (Var_Sub >= My_Sub'First and Var_Sub <= My_Sub'Last);
   pragma Assert (Var_Sub in My_Sub);

   Nondet_In_Type (Var_Sub);
   pragma Assert (Var_Sub = 1);
   pragma Assert (Var_Sub >= My_Sub'First and Var_Sub <= My_Sub'Last);
   pragma Assert (Var_Sub in My_Sub);
end Nondet_Functions;
