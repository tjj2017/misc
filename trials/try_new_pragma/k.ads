package K
is
   subtype Co_Ord is Integer range 0 .. 360;

   To_Be_Updated : Integer
     with Annotate => (ASVAT, Represents, "Try_New_Pragma.To_Be_Updated");
   X : Co_Ord;
   Y : Co_Ord;


--     pragma Import (Ada,
--                    Entity     => To_Be_Updated,
--                    External_Name => "Represents",
--                    Link_Name => "Try_New_Pragma.To_Be_Updated");
   pragma Import (Ada, X, "Represents",
                  "Try_New_Pragma.X:Try_New_Pragma.Co_Ord");
   pragma Import (Ada, Y, "Represents",
                  "Try_New_Pragma.Y:Try_New_Pragma.Co_Ord");

   procedure Update
     with Global => (In_Out => (X, Y, To_Be_Updated)),
     Annotate => (ASVAT, Nondet_In_Type),
     Import => True;
--           External_Name => "Nondet";
--     Convention => Ada;

--   pragma Global (In_Out => (X, Y, To_Be_Updated));
--   pragma Import (Ada, Update, "Nondet_In_Type");

--   pragma Annotate (ASVAT, Nondet_In_Type, Update);
--     Import => True,
--     Convention => Ada,
--     External_Name => "Nondet_In_Type";

--   function NonDet_Update return integer is (1);

--   function Update_2 return Integer is (1)
--     with Annotate => (Gnat2Goto, nondet, "fred");

end K;
