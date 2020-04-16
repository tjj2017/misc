procedure Try_Record_Of_Array is
   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;

   type Arr_T is array (Index) of Count;
   type Arr_R is record
      Arr : Arr_T;
   end record;

   R : Arr_R;
   A : Arr_T;
begin
   A (3) := 5;
   pragma Assert (A (3) = 5);

   R.Arr (3) := 5;
   pragma Assert (R.Arr (3) = 5);
end Try_Record_Of_Array;
