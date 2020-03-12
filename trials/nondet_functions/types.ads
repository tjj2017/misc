package Types is
   type My_Int is range 0 .. 1023;

   subtype My_Sub is My_Int range 0 .. 255;

   Var_Sub : My_Sub;
end Types;
