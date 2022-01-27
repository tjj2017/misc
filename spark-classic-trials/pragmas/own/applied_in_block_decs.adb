procedure Applied_In_Block_Decs is
begin
   declare
      pragma Classic_Own ((Plain => V));
      V : Integer;
   begin
      V := 0;
   end;
end Applied_In_Block_Decs;
