procedure Dynamic is
   type Arr_U is array (Integer range <>) of Integer;
   Low : Integer;
   High : Integer;
begin
   Low := 1;
   High := 10;
   declare
      subtype A is Arr_U (Low .. High);
      VA : A;
   begin
      VA (1) := 10;
      pragma Assert (VA (1) = 10);
   end;
end Dynamic;
