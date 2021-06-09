--  with Text_IO; use Text_IO;
procedure Arr_Assign is
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;

   A : Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
begin
   A := A (1 .. 3) & A (2 .. 8);
   for I in A'Range loop
--      Put (Integer'Image (A (I)) & ", ");
      if I <= 3 then
         pragma Assert (A (I) = I);
      else
         pragma Assert (A (I) = I - 2);
      end if;
   end loop;
--   New_Line;
end Arr_Assign;
