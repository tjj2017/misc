procedure Arr_Index is
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;

   A : Arr;
   I : Integer;
   X : Integer;
begin
   I := 1;
   A (I) := 23;
   pragma Assert (A (I) = 23);
   null;
end Arr_Index;
