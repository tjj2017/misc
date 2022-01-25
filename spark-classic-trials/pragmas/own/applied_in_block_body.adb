procedure Applied_In_Block_Body is
begin
   declare
      V : Integer;
   begin
      pragma Classic_Own ((Plain => V));
      V := 0;
   end;
end Applied_In_Block_Body;
