package My_Model is
   type Small is range 0 .. 63;
   subtype Smaller is Small range 1 .. 2;

   type My_Fixed_Array is array (Smaller) of Small;

   type My_Unconstrained_Array is array (Smaller range <>) of Small;

   type Array_Of_Arrays is array (Smaller) of My_Fixed_Array;

   type My_Enum is (one, two, three, four, five);
   subtype Sub_Enum is My_Enum range two .. four;

   type My_Mod is mod 2**5;

   type R is record
      S, T : Small;
   end record;


--     procedure Int_In_Type (I : in out Integer)
--     with Annotate => (ASVAT, Nondet),
--          Global   => null,
--          Import   => True;
--
   procedure Small_In_Type (M : in out Small)
   with Annotate => (ASVAT, Nondet_In_Type),
        Global   => null,
        Import   => True;

--     procedure My_Enum_In_Type (M :in out My_Enum)
--     with Annotate => (ASVAT, Nondet_In_Type),
--          Global   => null,
--          Import   => True;
--
--     procedure My_Mod_In_Type (M : in out My_Mod)
--     with Annotate => (ASVAT, Nondet_In_Type),
--          Global   => null,
--          Import   => True;

   procedure My_Fixed_Array_In_Type (A : in out My_Fixed_Array)
   with Annotate => (ASVAT, Nondet),
        Global   => null,
        Import   => True;

--     procedure My_Unconstrained_Array_In_Type (U : in out My_Unconstrained_Array)
--     with Annotate => (ASVAT, Nondet_In_Type),
--          Global   => null,
--          Import   => True;
--
--     procedure Array_Of_Arrays_In_Type (AA : in out Array_Of_Arrays)
--     with Annotate => (ASVAT, Nondet_In_Type),
--          Global   => null,
--          Import   => True;
--
--     procedure In_Type_R (Rec : in out R)
--     with Annotate => (ASVAT, Nondet_In_Type),
--          Global   => null,
--          Import   => True;

end My_Model;
