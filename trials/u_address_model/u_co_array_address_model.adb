procedure U_CO_Array_Address_Model is

   type My_Int is range 0 .. 100;
   type My_U_Array is array (Positive range  <>) of My_Int;

   type Pointer is access all My_U_Array;

   V : aliased My_U_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   PV : Pointer;
begin
   PV := V'Access;

   for I in V'Range loop
      pragma Assert (PV.all (I) = My_Int (I));
   end loop;

   for I in V'Range loop
      PV.all (I) := My_Int (V'Last - I + 1);
   end loop;

   for I in V'Range loop
      pragma Assert (V (I) = My_Int (V'Last - I + 1));
   end loop;

end U_CO_Array_Address_Model;
