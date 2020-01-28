with K; use K;
with System;
--  with nondet; use nondet;
procedure Try_New_Pragma (L : in out Integer) is
   subtype Co_Ord is Integer range 0 .. 360;
   X : Co_Ord;
   Y : Co_Ord;
   To_Be_Updated : Integer := 0;
--     Z : Integer;
--     The_Size : Integer;
--     My_Address : System.Address;

--     type A1 is array (1..10) of Integer;
--
--     type R is
--        record
--           A : Integer;
--           B : A1;
--        end record;
--
--     type A2 is array (1..10) of R;
--     type RR is
--        record
--           RecRec : R;
--           ArrRec : A2;
--        end record;
--
--     O_R : R := (7, (others => 3));
--     O_RR : RR := (O_R, (others => O_R));
--
--     function FI return Integer is (0);
--     function FR return R is (O_R);
--     function FRR return RR is (O_RR);


begin
   X := 1;
   Y := 2;
   Update;
   pragma Assert (X  <= Co_Ord'Last);
--   To_Be_Updated := NonDet_Update;
--   Z := Update_2;
--     pragma Assert (To_Be_Updated = 0);
--     Z := To_Be_Updated;
--     The_Size := To_Be_Updated'Size;
--     My_Address := Z'Address;

--     pragma Assert (To_Be_Updated'Valid);
--     pragma Assert (O_R.A'Valid);
--     pragma Assert (O_R.B (3)'Valid);
--     pragma Assert (O_RR.RecRec.A'Valid);
--     pragma Assert (O_RR.RecRec.B (5)'Valid);
--     pragma Assert (O_RR.ArrRec (4).A'Valid);
--     pragma Assert (O_RR.ArrRec (7).B (8)'Valid);
--     pragma Assert (FI'Valid);
--     pragma Assert (FR.A'Valid);
--     pragma Assert (FR.B (1)'Valid);
--     pragma Assert (FRR.RecRec.A'Valid);
--     pragma Assert (FRR.ArrRec (2).A'Valid);
end Try_New_Pragma;
