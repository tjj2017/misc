package body Part_Dec is
   procedure P (X : in out Partial_Dec) is
   begin
      X.A := X.A + 1;
   end P;
end Part_Dec;
