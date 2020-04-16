procedure Try_Arrays is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;

   type Arr_T is array (Index) of Count;
   type Arr_R is record
      Arr : Arr_T;
   end record;

   R : Arr_R;
   A : Arr_T;
begin
--     for I in Index loop
--        A (I) := Index'Last - I + 1;
--     end loop;
--
--     for I in Index loop
--        pragma Assert (A (I) = Index'Last - I + 1);
--     end loop;
--
--     for I in Index loop
--        R.Arr (I) := Index'Last - I + 1;
--     end loop;
--
--     for I in Index loop
--        pragma Assert (R.Arr (I) = Index'Last - I + 1);
--     end loop;

   A (3) := 5;
   pragma Assert (A (3) = 5);

   R.Arr (3) := 5;
   pragma Assert (R.Arr (3) = 5);
end Try_Arrays;
