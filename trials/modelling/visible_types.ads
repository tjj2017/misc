package Visible_Types is
   type My_Int is range -1024 .. 1023;

   subtype Sub_Int is Integer range 23 .. 29;
   subtype My_Sub_Int is My_Int range 0 .. 10;

end Visible_Types;
