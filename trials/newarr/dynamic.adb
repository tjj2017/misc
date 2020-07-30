procedure Dynamic is
   Low : Integer;
   High : Integer;
begin
   Low := 1;
   High := 10;
   declare
      type A is array (Low .. High) of Integer;
      VA : A;
   begin
      VA (1) := 10;
   end;
end Dynamic;
