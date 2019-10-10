procedure Try_New_Pragma (L : in out Integer) is
   subtype Co_Ord is Integer range 0 .. 364;
   X : Co_Ord;
   Y : Co_Ord;

   procedure Q is

      procedure P;
      pragma Global (In_Out => (X, Y, L));
      pragma Import (Convention     => Ada,
                     Entity         => P,
                     External_Name  => "Non_Det_In_Type");
   begin
      P;
   end Q;

begin
   X := 1;
   Y := 2;
   Q;
end Try_New_Pragma;
