procedure Nested_Package is
   package Nested is
      pragma Classic_Own ((Plain => V));
      pragma Classic_Initializes ((V));
      V : Integer;
   end Nested;
begin
   Nested.V := 0;
end Nested_Package;
