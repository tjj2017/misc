package body Nested_Within is

    --# inherit Nested_Within;
   package Two is
      procedure R (X : in out Integer);
   end Two;

   --# inherit Two;
   package One is
      procedure Q (X : in out Integer);
   end One;

   ---------
   -- Inc --
   ---------
   function Inc (X : Integer) return Integer is
   begin
      return X + 1;
   end Inc;

   package body Two is
      procedure R (X : in out Integer) is
      begin
         X := Nested_Within.Inc (X);
      end R;
   end Two;

   package body One is
      procedure Q (X : in out Integer) is
      begin
         Two.R (X);
      end Q;
   end One;

   -------
   -- P --
   -------

   procedure P (X : in out Integer) is
   begin
      One.Q (X);
   end P;

end Nested_Within;
