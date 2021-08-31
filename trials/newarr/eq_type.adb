procedure Eq_Type is
   subtype Index is Integer range 1 .. 10;

   type U_Arr is Array (Integer range <>) of Integer;
   function Fun1 return U_Arr is (1, 2, 3, 4, 5);
begin
   pragma Assert (Fun1 (Fun1'First + 1) = 2);
end Eq_Type;
