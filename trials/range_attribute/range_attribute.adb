procedure Range_Attribute is
   subtype S is Integer range 1 .. 10;
   type A_T is array (S) of Integer;
   A : A_T;
begin
   for I in S loop
      A (I) := I;
   end loop;

   for J in A'Range loop
      A (J) := A (J) + J;
   end loop;

   pragma Assert (A (3) = 6);

end Range_Attribute;
