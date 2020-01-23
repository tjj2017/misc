procedure Try_Nondet_Func is
   type Enum is (one, two, three);

   type R is new Float range -1.0 .. 10.0;

   function My_Nondet (X, Y , Z : Integer; Yuk : out Integer) return Integer
     with Annotate => (ASVAT, Nondet),
          Global   => Null
   is
   begin
      Yuk := X + Y;
      pragma Assert (Yuk > 0);
      return Yuk + Z;
   end My_Nondet;

   I, J : Integer;
begin
   pragma Assert (Enum'Pos (two) in
                    Enum'Pos(Enum'First) .. Enum'Pos(Enum'Last));
   pragma Assert (-1.0 in R'First .. R'Last);
   I := My_Nondet (1, 2, 3, J);
end Try_Nondet_Func;
