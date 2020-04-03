with System;
procedure Vari is
   subtype Count is Integer range 0 .. 10;
--     subtype Index is Count range Count'First .. Count'last - 1;
--     type AF is array (Index) of Integer;
--     type F is record
--        C : Count;
--        D : AF;
--     end record;
--
--     type AV is array (Index range  <>) of Integer;
--     type V (N : Index) is record
--        C : Count;
--        D : AV (Count'First .. N);
--     end record;
--
--     subtype VF is V (Count'Last - 1);
--
--     V_F : F := (0, (others => 0));
--     V_V : VF := (Count'Last - 1, 0, (others => 0));
   procedure With_Address (A : System.Address)
     with Annotate => (ASVAT, Memcpy),
     Import;

   C : Count := 5;
begin
   With_Address (C'Address);
end Vari;
