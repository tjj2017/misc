procedure Multi_Applications is
   function Inc (I : Integer) return Integer is (I + 1);

   type R is record
      A, B : Integer;
   end record;

   type RR is access all R;

   function Inc_R (V : R) return R is ((Inc (V.A), Inc (V.B)));
   function Inc_RR (V : RR) return RR is
   begin
      V.all := R'(Inc (V.A), Inc (V.B));
      return V;
   end Inc_RR;


   X : Integer := 1;
   Y : aliased R := (1, 1);
begin
--     X := Inc (X);
--     pragma Assert (X = 2);
--     X := Inc (X);
--     pragma Assert (X = 3);

--     X := Inc (X) + Inc (X + 1);
--     pragma Assert (X = 9);

--     X := X + Inc (Inc (X));
--     pragma Assert (X = 4);

--     pragma Assert (X + Inc (X) + Inc (X) = 5);
--     X := Inc_R (Y).A;
--     pragma Assert (X = 2);

--     X := Inc (Inc_R (Y).B);
--     pragma Assert (X = 3);

--     X := Inc_R ((2, 3)).B;
--     pragma Assert (X = 4);

   X := Inc_RR (Y'Access).A;
   pragma Assert (X = 2);
end Multi_Applications;
