package Hidden_Types is
   type My_Int is range -1024 .. 1023;
   type My_Enum is (one, two, three, four, five);

   subtype My_Sub_Int is My_Int range 0 .. 10;
   subtype My_Sub_Enum is My_Enum range two .. four;

   Hid_Int : Integer;
   Hid_My_Int : My_Int;
   Hid_My_Enum : My_Enum;
   Hid_My_Sub_Int : My_Sub_Int;
   Hid_My_Sub_Enum : My_Sub_Enum;

end Hidden_Types;
