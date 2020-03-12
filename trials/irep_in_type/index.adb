procedure Index is
   type A is array (1 .. 10) of Integer;
   A_V : A := (others => 0);
begin
   A_V (1) := 1;
end Index;
