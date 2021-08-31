procedure Simple_Multi_Applications is
   type R is record
      A, B : Integer;
   end record;

   type RR is access all R;

   V : RR;
   Y : aliased R;
   Z : R := (1, 1);
begin
   V := Y'Access;
   V.all := Z; --  R'(2, 2);
end Simple_Multi_Applications;
