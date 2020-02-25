with Visible_Types; use type Visible_Types.My_Int, Visible_Types.My_Enum;
with Visible_Vars;
with Hidden_Vars;
with Hidden_Types;  use type Hidden_Types.My_Int, Hidden_Types.My_Enum;
with The_Model; use The_Model;
procedure Modelling is
   Local_Int_1, Local_Int_2, Local_Int_3 : Visible_Types.My_Sub_Int;
begin
   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
   Visible_Vars.Var_My_Sub_Int := 6;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_My_Enum := Visible_Types.four;
   Hidden_Vars.Var_My_Sub_Int := 6;
   Hidden_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Types.Hid_Int := 0;
   Hidden_Types.Hid_My_Int := 1;
   Hidden_Types.Hid_My_Enum := Hidden_Types.four;
   Hidden_Types.Hid_My_Sub_Int := 6;
   Hidden_Types.Hid_My_Sub_Enum := Hidden_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Update_Nondet_Anno;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
   Visible_Vars.Var_My_Sub_Int := 6;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_My_Enum := Visible_Types.four;
   Hidden_Vars.Var_My_Sub_Int := 6;
   Hidden_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Types.Hid_Int := 0;
   Hidden_Types.Hid_My_Int := 1;
   Hidden_Types.Hid_My_Enum := Hidden_Types.four;
   Hidden_Types.Hid_My_Sub_Int := 6;
   Hidden_Types.Hid_My_Sub_Enum := Hidden_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Update_Nondet_Import;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
   Visible_Vars.Var_My_Sub_Int := 6;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_My_Enum := Visible_Types.four;
   Hidden_Vars.Var_My_Sub_Int := 6;
   Hidden_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Types.Hid_Int := 0;
   Hidden_Types.Hid_My_Int := 1;
   Hidden_Types.Hid_My_Enum := Hidden_Types.four;
   Hidden_Types.Hid_My_Sub_Int := 6;
   Hidden_Types.Hid_My_Sub_Enum := Hidden_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

  Update_Nondet_In_Type_Anno;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum >=
                    Visible_Types.My_Sub_Enum'First and
                      Visible_Vars.Var_My_Sub_Enum <=
                        Visible_Types.My_Sub_Enum'Last);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_My_Enum := Visible_Types.four;
   Visible_Vars.Var_My_Sub_Int := 6;
   Visible_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_My_Enum := Visible_Types.four;
   Hidden_Vars.Var_My_Sub_Int := 6;
   Hidden_Vars.Var_My_Sub_Enum := Visible_Types.two;

   Hidden_Types.Hid_Int := 0;
   Hidden_Types.Hid_My_Int := 1;
   Hidden_Types.Hid_My_Enum := Hidden_Types.four;
   Hidden_Types.Hid_My_Sub_Int := 6;
   Hidden_Types.Hid_My_Sub_Enum := Hidden_Types.two;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Update_Nondet_In_Type_Import;
   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_My_Enum = Visible_Types.four);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum = Visible_Types.two);

   pragma Assert (Hidden_Types.Hid_Int = 0);
   pragma Assert (Hidden_Types.Hid_My_Int = 1);
   pragma Assert (Hidden_Types.Hid_My_Enum = Hidden_Types.four);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int = 6);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum = Hidden_Types.two);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                 Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_My_Enum in Visible_Types.My_Enum);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Enum in Visible_Types.My_Sub_Enum);

   pragma Assert (Hidden_Types.Hid_Int in Integer);
   pragma Assert (Hidden_Types.Hid_My_Int >= Hidden_Types.My_Int'First and
                 Hidden_Types.Hid_My_Int <= Hidden_Types.My_Int'Last);
   pragma Assert (Hidden_Types.Hid_My_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Int in Hidden_Types.My_Sub_Int);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Enum);
   pragma Assert (Hidden_Types.Hid_My_Sub_Enum in Hidden_Types.My_Sub_Enum);

   Local_Int_1 := 3;
   Local_Int_2 := 5;
   Local_Int_3 := 7;

   pragma Assert (Local_Int_1 = 3);
   pragma Assert (Local_Int_2 = 5);
   pragma Assert (Local_Int_3 = 7);

   pragma Assert (Local_Int_1 in Visible_Types.My_Sub_Int);
   pragma Assert (Local_Int_2 in Visible_Types.My_Sub_Int);
   pragma Assert (Local_Int_3 in Visible_Types.My_Sub_Int);

   Local_Int_1 := Nondet_Integer_Anno;
   Local_Int_2 := Nondet_Integer_Import;
   Local_Int_3 := Nondet_Integer_With_Body (Local_Int_3);

   pragma Assert (Local_Int_1 = 3);
   pragma Assert (Local_Int_2 = 5);
   pragma Assert (Local_Int_3 = 7);

   pragma Assert (Local_Int_1 in Visible_Types.My_Sub_Int);
   pragma Assert (Local_Int_2 in Visible_Types.My_Sub_Int);
   pragma Assert (Local_Int_3 in Visible_Types.My_Sub_Int);

end Modelling;
