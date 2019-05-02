package Private_Type is
   type P is private;
   P_Const : constant P;
private
   type P is new Integer range 1 .. 10;
   P_Const : constant P := 5;
end Private_Type;
