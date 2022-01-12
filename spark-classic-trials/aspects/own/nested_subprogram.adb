procedure Nested_Subprogram is
   procedure P
   with Classic_Own => ((Plain => V))
   is
      V : Integer;
   begin
      V := 0;
   end P;
   function F return Integer
   with Classic_Own => ((Plain => V))
   is
      V : Integer;
   begin
      V := 0;
      return V;
   end F;
begin
   null;
end Nested_Subprogram;
