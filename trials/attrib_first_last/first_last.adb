procedure First_Last is
   subtype S is Integer range 1..10;

   First, Last : Integer;
begin
   First := S'First;
   Last := S'Last;

   pragma Assert (First = 1);
   pragma Assert (Last = 10);
end First_Last;
