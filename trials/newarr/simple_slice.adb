procedure Simple_Slice is
   type U_Array is array (Integer range <>) of Integer;

   subtype Short_U_Array is U_Array (1 .. 5);
   subtype Long_U_Array  is U_Array (1 .. 20);

   LU : Long_U_Array  := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, others => 0);
   SU : Short_U_Array := LU (6 .. 10);
begin
   for I in SU'Range loop
      pragma Assert (SU (I) = I + 5);
   end loop;
end Simple_Slice;
