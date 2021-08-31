--  with Numeric;
pragma Extend_System (Aux_Dec);
with System; use System;
procedure Types_1 is
   TYPE id_nums IS
   --  The type Unsigned_8 does not exist in package System.
   --  It exists only in the system extention package Aux_Dec
      RECORD
         num1 : Unsigned_8;
         num2 : System.Unsigned_8;
         num3 : Unsigned_8;
         num4 : Unsigned_8;
      END RECORD;
begin
   pragma Assert (id_nums'size = 32);
end Types_1;
