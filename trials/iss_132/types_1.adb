with Numeric;
procedure Types_1 is
   TYPE id_nums IS
      RECORD
         num1 : numeric.types.uinteger8s;
         num2 : numeric.types.uinteger8s;
         num3 : numeric.types.uinteger8s;
         num4 : numeric.types.uinteger8s;
      END RECORD;
begin
   pragma Assert (id_nums'size = 32);
end Types_1;
