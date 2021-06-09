procedure Arrays_Assign_Nonoverlap is
   type Arr_Long is array (1..4) of Integer;
   Full_Arr : Arr_Long := (1,2,3,4);
   Part1_Arr : Arr_Long := (1,2,0,0);
   Part2_Arr : Arr_Long := (3,4,0,0);

   type Arr_Arr is array (1 .. 3) of Arr_Long;
   AA : Arr_Arr;
   I : Integer;
begin
   I :=  AA (1) (1 ..2)'First;
   pragma Assert (I = 1);

   AA (1) (1 .. 2) := (5, 6);
   Full_Arr := AA (1) (1 ..2) & Part2_Arr(1..0) & Part1_Arr(1..2);
   pragma Assert (Full_Arr (1) = 5);
   pragma Assert (Full_Arr (2) = 6);
   pragma Assert (Full_Arr (3) = 1);
   pragma Assert (Full_Arr (4) = 2);

   Full_Arr := Part2_Arr(1..3) (1 .. 2) & Part2_Arr(1..0) & Part1_Arr(1..2);
   pragma Assert (Full_Arr(1)=3);
   pragma Assert (Full_Arr(2)=4);
   pragma Assert (Full_Arr(3)=1);
   pragma Assert (Full_Arr(4)=2);

   AA (1) := Part2_Arr(1..3) (1 .. 2) & Part2_Arr(1..0) & Part1_Arr(1..2);
   AA (2) := Full_Arr;
   pragma Assert (AA (1) (1)=3);
   pragma Assert (AA (1) (2)=4);
   pragma Assert (AA (1) (3)=1);
   pragma Assert (AA (1) (4)=2);

   pragma Assert (AA (2) (1)=3);
   pragma Assert (AA (2) (2)=4);
   pragma Assert (AA (2) (3)=1);
   pragma Assert (AA (1) (4)=2);

end Arrays_Assign_Nonoverlap;
