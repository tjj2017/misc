with Visible_Types; use type Visible_Types.My_Int, Visible_Types.My_Enum;
with Visible_Vars;
with The_Model; use The_Model;
procedure Modelling is
begin
   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
--   Visible_Vars.Var_My_Real := 2.0;
--   Visible_Vars.Var_My_Fixed := Pi/4.0;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
--   Visible_Vars.Var_Sub_Int := 5;
--   Visible_Vars.Var_My_Sub_Int := 6;
--   Visible_Vars.Var_My_Sub_Real := 7.0;
--   Visible_Vars.Var_My_Sub_Fixed := Pi/8.0;
--   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
--   pragma Assert (Visible_Vars.Var_My_Real in Visible_Types.My_Real);
--   pragma Assert (Visible_Vars.Var_My_Fixed in Visible_Types.My_Fixed);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_Sub_Int in 0 .. 23);
--   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
--   pragma Assert (Visible_Vars.Var_My_Sub_Real in Visible_Types.My_Sub_Real);
--   pragma Assert (Visible_Vars.Var_My_Sub_Fixed in Visible_Types.My_Sub_Fixed);
--   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Update_Nondet_Visible_Anno;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
--   Visible_Vars.Var_My_Real := 2.0;
--   Visible_Vars.Var_My_Fixed := Pi/4.0;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
--   Visible_Vars.Var_Sub_Int := 5;
   Visible_Vars.Var_My_Sub_Int := 6;
--   Visible_Vars.Var_My_Sub_Real := 7.0;
--   Visible_Vars.Var_My_Sub_Fixed := Pi/8.0;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_My_Real in Visible_Types.My_Real);
--   pragma Assert (Visible_Vars.Var_My_Fixed in Visible_Types.My_Fixed);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_Sub_Int in 0 .. 23);
--   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
--   pragma Assert (Visible_Vars.Var_My_Sub_Real in Visible_Types.My_Sub_Real);
--   pragma Assert (Visible_Vars.Var_My_Sub_Fixed in Visible_Types.My_Sub_Fixed);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Update_Nondet_Visible_Import;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
--   Visible_Vars.Var_My_Real := 2.0;
--   Visible_Vars.Var_My_Fixed := Pi/4.0;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
--   Visible_Vars.Var_Sub_Int := 5;
   Visible_Vars.Var_My_Sub_Int := 6;
--   Visible_Vars.Var_My_Sub_Real := 7.0;
--   Visible_Vars.Var_My_Sub_Fixed := Pi/8.0;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
--   pragma Assert (Visible_Vars.Var_My_Real in Visible_Types.My_Real);
--   pragma Assert (Visible_Vars.Var_My_Fixed in Visible_Types.My_Fixed);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_Sub_Int in 0 .. 23);
--   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
--   pragma Assert (Visible_Vars.Var_My_Sub_Real in Visible_Types.My_Sub_Real);
--   pragma Assert (Visible_Vars.Var_My_Sub_Fixed in Visible_Types.My_Sub_Fixed);
--   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Update_Nondet_In_Type_Visible_Anno;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum >=
                    Visible_Types.My_Sub_Enum'First and
                      Visible_Vars.Var_My_Sub_Enum >=
                        Visible_Types.My_Sub_Enum'Last);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
--   Visible_Vars.Var_My_Real := 2.0;
--   Visible_Vars.Var_My_Fixed := Pi/4.0;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
--   Visible_Vars.Var_Sub_Int := 5;
   Visible_Vars.Var_My_Sub_Int := 6;
--   Visible_Vars.Var_My_Sub_Real := 7.0;
--   Visible_Vars.Var_My_Sub_Fixed := Pi/8.0;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_My_Real in Visible_Types.My_Real);
--   pragma Assert (Visible_Vars.Var_My_Fixed in Visible_Types.My_Fixed);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
--   pragma Assert (Visible_Vars.Var_Sub_Int in 0 .. 23);
--   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
--   pragma Assert (Visible_Vars.Var_My_Sub_Real in Visible_Types.My_Sub_Real);
--   pragma Assert (Visible_Vars.Var_My_Sub_Fixed in Visible_Types.My_Sub_Fixed);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   Update_Nondet_In_Type_Visible_Import;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);
end Modelling;
