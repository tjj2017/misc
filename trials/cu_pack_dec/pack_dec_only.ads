package Pack_Dec_Only is

   type My_Int is range -2**16 .. (2**16 - 1);

   type Enum is (one, two, three);

   E : Enum := one;
end Pack_Dec_Only;
