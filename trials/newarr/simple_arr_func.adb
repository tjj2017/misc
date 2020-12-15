procedure Simple_Arr_Func is
--     subtype Index is Integer range 1 .. 10;
--     subtype Sub_Index is Index range 9 .. 9;
--     type Arr is array (Index) of Integer;
--
   type U_Arr is array (Integer range <>) of Integer;

   function Fun_1 (L, U : Integer; V : Integer) return U_Arr is (L .. U => V);

   A : U_Arr := Fun_1 (1, 4, 3);
begin
   pragma Assert (A (1) = 3);
end Simple_Arr_Func;
