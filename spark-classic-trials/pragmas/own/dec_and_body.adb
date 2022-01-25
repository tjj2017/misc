package body Dec_and_Body is

   -------
   -- P --
   -------

   procedure P is
   begin
      V := V + 1;
   end P;

   pragma Classic_Own ((Plain => V));
begin
--     pragma Classic_Own ((Plain => V));
   V := 0;
end Dec_and_Body;
