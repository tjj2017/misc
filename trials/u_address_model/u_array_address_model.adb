with System;  use type System.Address;
with System.Address_To_Access_Conversions;
procedure U_Array_Address_Model is

   type My_Int is range 0 .. 100;
   type My_U_Array is array (Positive range  <>) of My_Int;
   type My_C_Array is array (5 .. 15) of My_Int;

   type Pointer_U is access all My_U_Array;
   type Pointer_C is access all My_C_Array;

   package Unconstrained_Address_To_Access_Conversions is new
     System.Address_To_Access_Conversions (My_U_Array);
   use Unconstrained_Address_To_Access_Conversions;

   package Constrained_Address_To_Access_Conversions is new
     System.Address_To_Access_Conversions (My_C_Array);

   V_U  : My_U_Array (1 ..10);
   PV_U : Pointer_U;

   V_C  : My_C_Array;
   PV_C : Pointer_C;
begin
   for I in V_U'Range loop
      --  An Ada check assertion fails here but it should not.
      --  A problem with Ada check assertions.
      V_U (I) := My_Int (I);
   end loop;

   PV_U := Pointer_U (To_Pointer (V_U'Address));

   for I in V_U'Range loop
      --  The assertion succeeds because PV is really My_Int.
      pragma Assert (PV_U.all (I) = My_Int (I));
   end loop;

   for I in V_U'Range loop
      PV_U.all (I) := My_Int (V_U'Last - I + 1);
   end loop;

   for I in V_U'Range loop
      --  The assertion succeeds because PV is really My_Int.
      pragma Assert (V_U (I) = My_Int (V_U'Last - I + 1));
   end loop;

   pragma Assert (V_U'Address = To_Address (Object_Pointer (PV_U)));

   for I in V_C'Range loop
      --  An Ada check assertion fails here but it should not.
      --  A problem with Ada check assertions.
      V_C (I) := My_Int (I - V_C'First + 1);
   end loop;

   PV_C := Pointer_C
     (Constrained_Address_To_Access_Conversions.To_Pointer (V_C'Address));

   for I in V_C'Range loop
      --  The assertion succeeds because PV is really My_Int.
      pragma Assert (PV_C.all (I) = My_Int (I));
   end loop;

   for I in V_C'Range loop
      PV_C.all (I) := My_Int (V_C'Last - V_C'First - I + 1);
   end loop;

   for I in V_C'Range loop
      --  The assertion succeeds because PV is really My_Int.
      pragma Assert (V_C (I) = My_Int (V_C'Last - V_C'First - I + 1));
   end loop;

   pragma Assert
     (V_C'Address =
       Constrained_Address_To_Access_Conversions.To_Address
         (Constrained_Address_To_Access_Conversions.Object_Pointer (PV_C)));
end U_Array_Address_Model;
