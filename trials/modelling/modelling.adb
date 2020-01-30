with Visible_Types; use type Visible_Types.My_Int;
with Visible_Vars;
with Hidden_Vars;  -- Only hidden from the model not the main program
with Hidden_Types_And_Vars; -- Only hidden from the model
use type Hidden_Types_And_Vars.My_Int;

with The_Model; use The_Model;
procedure Modelling is
begin
   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_Sub_Int := 25;
   Visible_Vars.Var_My_Sub_Int := 6;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_Sub_Int := 25;
   Hidden_Vars.Var_My_Sub_Int := 6;

   Hidden_Types_And_Vars.Var_My_Int := 0;
   Hidden_Types_And_Vars.Var_My_Int := 1;
   Hidden_Types_And_Vars.Var_Sub_Int := 25;
   Hidden_Types_And_Vars.Var_My_Sub_Int := 6;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Hidden_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Update_Nondet_Anno;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_Sub_Int := 25;
   Visible_Vars.Var_My_Sub_Int := 6;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_Sub_Int := 25;
   Hidden_Vars.Var_My_Sub_Int := 6;

   Hidden_Types_And_Vars.Var_My_Int := 0;
   Hidden_Types_And_Vars.Var_My_Int := 1;
   Hidden_Types_And_Vars.Var_Sub_Int := 25;
   Hidden_Types_And_Vars.Var_My_Sub_Int := 6;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Update_Nondet_Import;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_Sub_Int := 25;
   Visible_Vars.Var_My_Sub_Int := 6;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_Sub_Int := 25;
   Hidden_Vars.Var_My_Sub_Int := 6;

   Hidden_Types_And_Vars.Var_My_Int := 0;
   Hidden_Types_And_Vars.Var_My_Int := 1;
   Hidden_Types_And_Vars.Var_Sub_Int := 25;
   Hidden_Types_And_Vars.Var_My_Sub_Int := 6;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Update_Nondet_In_Type_Anno;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

   Visible_Vars.Var_Int := 0;
   Visible_Vars.Var_My_Int := 1;
   Visible_Vars.Var_Sub_Int := 25;
   Visible_Vars.Var_My_Sub_Int := 6;

   Hidden_Vars.Var_Int := 0;
   Hidden_Vars.Var_My_Int := 1;
   Hidden_Vars.Var_Sub_Int := 25;
   Hidden_Vars.Var_My_Sub_Int := 6;

   Hidden_Types_And_Vars.Var_My_Int := 0;
   Hidden_Types_And_Vars.Var_My_Int := 1;
   Hidden_Types_And_Vars.Var_Sub_Int := 25;
   Hidden_Types_And_Vars.Var_My_Sub_Int := 6;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);

  Update_Nondet_In_Type_Import;

   pragma Assert (Visible_Vars.Var_Int = 0);
   pragma Assert (Visible_Vars.Var_My_Int = 1);
   pragma Assert (Visible_Vars.Var_Sub_Int = 25);
   pragma Assert (Visible_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Visible_Vars.Var_Int in Integer);
   pragma Assert (Visible_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Visible_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Visible_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);
   pragma Assert (Hidden_Vars.Var_Int = 0);
   pragma Assert (Hidden_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Vars.Var_Int in Integer);
   pragma Assert (Hidden_Vars.Var_My_Int >= Visible_Types.My_Int'First and
                    Visible_Vars.Var_My_Int <= Visible_Types.My_Int'Last);
   pragma Assert (Hidden_Vars.Var_Sub_Int in Visible_Types.Sub_Int);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int in Visible_Types.My_Sub_Int);

   pragma Assert (Hidden_Types_And_Vars.Var_Int = 0);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int = 1);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int = 25);
   pragma Assert (Hidden_Vars.Var_My_Sub_Int = 6);

   pragma Assert (Hidden_Types_And_Vars.Var_Int in Hidden_Types_And_Vars.Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Int >=
                    Hidden_Types_And_Vars.My_Int'First and
                      Hidden_Types_And_Vars.Var_My_Int <=
                        Hidden_Types_And_Vars.My_Int'Last);
   pragma Assert (Hidden_Types_And_Vars.Var_Sub_Int in
                    Hidden_Types_And_Vars.Sub_Int);
   pragma Assert (Hidden_Types_And_Vars.Var_My_Sub_Int in
                    Hidden_Types_And_Vars.My_Sub_Int);


end Modelling;
