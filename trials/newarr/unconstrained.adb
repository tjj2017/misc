procedure Unconstrained is
   subtype Index is Integer range 1 .. 10;

   type U_Arr is Array (Integer range <>) of Integer;
   --  This declaration currently raises an unsupported report
   --  as the function returns an unconstrained array type
   function Fun1 return U_Arr is (1, 2, 3, 4, 5);
   function Fun2 (L, U : Index; V : Integer) return U_Arr is (L .. U => V);
   function Fun3 (V : Integer) return U_Arr is (1 .. 10 => V);

   subtype US is U_Arr (1 .. 5);
   Low, High : Integer;
   A : U_Arr (1 ..5) :=  Fun1;
   B : U_Arr := Fun2 (1, 10, 19);
begin
   Low := 1;
   High := 10;
   pragma Assert (Fun1'First = Integer'First);
   pragma Assert (Fun1'Length = 5);
   pragma Assert (Fun1'Last = Fun1'First + Fun1'Length - 1);
   pragma Assert (Fun2 (Low, High, 101)'First = 1);
   pragma Assert (Fun2 (Low, High, 103)'Last = High);
   pragma Assert (Fun2 (Low, High, 105)'Length = High - Low + 1);
   pragma Assert (Fun3 (17)'First = 1);
   pragma Assert (Fun3 (17)'Last = 10);
   pragma Assert (Fun1 (Fun1'First + 1) = 2);
   pragma Assert (Fun2 (Low, High, 107) (Low + 2) = 107);
   pragma Assert (Fun3 (17) (2) = 17);
   for I in A'Range loop
      pragma Assert (A (I) = I);
   end loop;
   for I in 1 .. 10 loop
      pragma Assert (B (I) = 19);
   end loop;
   null;
end Unconstrained;
