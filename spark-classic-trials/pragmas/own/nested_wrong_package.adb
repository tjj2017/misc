procedure Nested_Wrong_Package is
   package Nested is
      pragma Classic_Initializes ((V));
      pragma Classic_Own ((Plain => V));
      V : Integer;
   end Nested;
begin
   Nested.V := 0;
end Nested_Wrong_Package;
