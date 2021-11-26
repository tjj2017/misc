package Visible_Types is
   type My_Int is range -1024 .. 1023;
--   type My_Real is new Float range -6000.0 .. 5999.9;
--   type My_Fixed is delta 0.1 range -1.0 .. +1.0;

   type My_Enum is (one, two, three, four, five);

   subtype My_Sub_Int is My_Int range 0 .. 10;
--   subtype My_Sub_Real is My_Real range -25.0 .. +25.0;
--   subtype My_Sub_Fixed is My_Fixed range 0.0 .. 1.0;

   subtype My_Sub_Enum is My_Enum range two .. four;
end Visible_Types;
