procedure New_Incomlete_Dec is
   type Partial_Dec;

   procedure Inc (X : in out Partial_Dec);

   type Partial_Dec is record
      A : Integer;
   end record;

   procedure Inc (X : in out Partial_Dec) is
   begin
      X.A := X.A + 1;
   end Inc;

   V : Partial_Dec := (A => 19);

begin
   Inc (V);
   pragma Assert (V.A = 20);
end New_Incomlete_Dec;
