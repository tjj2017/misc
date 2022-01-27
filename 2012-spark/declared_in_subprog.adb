package body Declared_In_Subprog is

   -------
   -- P --
   -------

   procedure P (X : in out Integer) is
      --# inherit Declared_In_Subprog;
      package Within
      is
         procedure Q (X : in out Integer);
         --# global out Declared_In_Subprog.V;
      end Within;
      package body Within is
         procedure Q (X : in out Integer) is
         begin
            Declared_In_Subprog.V := X;
            X := X + 1;
         end Q;
      end Within;
   begin
      Within.Q (X);
   end P;

end Declared_In_Subprog;
