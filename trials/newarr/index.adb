procedure Index is
   type Arr is array (1 .. 5) of integer;
   type Arr_Ptr is access all Arr;

   A : aliased Arr := (1, 2, 3, 4, 5);
   B : aliased Arr := (6, 7, 8, 9, 10);

   A_Ptr : Arr_Ptr;
   B_Ptr : aliased Arr_Ptr;

   function Indexer (A : Arr; I : Integer) return Integer;
   function Indexer (A : Arr; I : Integer) return Integer is
   begin
      pragma Assert (I > 0);
      pragma Assert (A (I) = I);
      return A (I);
   end Indexer;

begin
   A_Ptr := A'Access;
   B_Ptr := B'Access;

   pragma Assert (A_Ptr (2) = 2);
   pragma Assert (B_Ptr (3) = 8);

   pragma Assert (Indexer (A, 2) = 2);
end Index;
