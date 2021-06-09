procedure Concat is
   type U_Arr is array (Positive range <>) of Integer;

   U1 : constant U_Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   U2 : U_Arr (1 .. 10);
begin
   U2 := U1 (1 .. 3) & U1 (4 .. 5) & U1 (6 .. 9) & U1 (10);
   for I in U2'Range loop
      pragma Assert (U2 (I) = I);
   end loop;
end Concat;
