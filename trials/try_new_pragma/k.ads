package K
is
   subtype Co_Ord is Integer range 0 .. 360;

   To_Be_Updated : Integer;
   X : Co_Ord;
   Y : Co_Ord;

   pragma Import (Ada,
                  Entity     => To_Be_Updated,
                  External_Name => "Replace_With",
                  Link_Name => "Try_New_Pragma.To_Be_Updated");
   pragma Import (Ada, X, "Replace_With",
                  "Try_New_Pragma.X:Try_New_Pragma.Co_Ord");
   pragma Import (Ada, Y, "Replace_With",
                  "Try_New_Pragma.Y:Try_New_Pragma.Co_Ord");

   procedure Update;
   pragma Global (In_Out => (X, Y, To_Be_Updated));
   pragma Import (Ada, Update, "Nondet_In_Type");

--   function NonDet_Update return integer is (1);

--   function Update_2 return Integer is (1)
--     with Annotate => (Gnat2Goto, nondet, "fred");

end K;
