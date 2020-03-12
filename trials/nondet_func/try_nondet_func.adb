procedure Try_Nondet_Func (Out_Param : in out Integer) is
   type Enum is (one, two, three);

   type R is new Float range -1.0 .. 10.0;

   subtype Small is Integer range 1 .. 10;

   function My_Nondet (X, Y , Z : Integer; Yuk : out Integer) return Small
     with Annotate => (ASVAT, Nondet),
     Global   => Null
   is
   begin
      Yuk := X + Y;
      pragma Assert (Yuk > 0);
      return Yuk + Z;
   end My_Nondet;


   I : Small;
   J : Integer;

   procedure P_Nondet (V : in out Integer)
     with Annotate => (ASVAT, Nondet),
     Global => (In_Out => I),
     Import;

begin
   pragma Assert (Enum'Pos (two) in
                    Enum'Pos(Enum'First) .. Enum'Pos(Enum'Last));
   pragma Assert (-1.0 in R'First .. R'Last);

   Out_Param := 5;
   I := 1;
   J := 3;
   pragma Assert (I = 1);
   pragma Assert (J = 3);
   pragma Assert (I in Small);
   I := My_Nondet (1, 2, 3, J);
   pragma Assert (I = 1);
   pragma Assert (J = 3);
   pragma Assert (I in Small);

   I := 1;
   J := 3;
   pragma Assert (I = 1);
   pragma Assert (J = 3);
   pragma Assert (I in Small);
   P_Nondet (J);
   pragma Assert (I = 1);
   pragma Assert (J = 3);
   pragma Assert (I in Small);
end Try_Nondet_Func;
