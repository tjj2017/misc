procedure Nested_Package is
   package P
   with Classic_Own => ((Plain => V)), Classic_Initializes => (V)
   is
      V : Integer;
   end P;
begin
   null;
end Nested_Package;
