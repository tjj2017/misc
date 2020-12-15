procedure Simple_Slice is
   type U_Array is array (Integer range <>) of Integer;

   subtype Short_U_Array is U_Array (1 .. 5);
   subtype Long_U_Array  is U_Array (1 .. 20);

   SU : Short_U_Array;
   LU : Long_U_Array;
begin
   pragma Assert (SU = LU (6 .. 10));
end Simple_Slice;
