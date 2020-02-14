procedure Enums is
   type My_Enum is (one, two, three, four, five);

   subtype Enum_Sub is My_Enum range two .. four;

   VE : My_Enum := one;
   VS : Enum_Sub := two;
--   T  : My_Enum;
begin
--     VE := one;
--     VS := two;
--     T  := VE;
--     VE := VS;
--     VS := VE;
--     VE := T;
   pragma Assert (VE in My_Enum);
   pragma Assert (VS in My_Enum);
   pragma Assert (VE <= VS);
   pragma Assert (VE <= My_Enum'Last);
   pragma Assert (VE >= My_Enum'First);
   pragma Assert (VE in Enum_Sub);
   pragma Assert (VE >= Enum_Sub'First);
end Enums;
