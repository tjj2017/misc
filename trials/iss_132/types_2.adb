with Numeric.Types_2;
procedure Types_2 is
   TYPE id_nums IS
      RECORD
         num1 : numeric.types_2.uinteger8s;
         num2 : numeric.types_2.uinteger8s;
         num3 : numeric.types_2.uinteger8s;
         num4 : numeric.types_2.uinteger8s;
      END RECORD;
begin
   pragma Assert (id_nums'size = 32);
end Types_2;
