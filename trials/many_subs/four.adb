package body Four is
   procedure Four_One (X : in out Integer) is
   begin
      null;
   end Four_One;

   procedure Four_Two (X : in out Integer) is
   begin
      Four_One (X);
   end Four_Two;
end Four;

