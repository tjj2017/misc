package A_Named_Number.Child is
   pragma Classic_Own ((Plain => N1, Plain => N));
   N1 : constant Integer := N;
   N : constant := -3;
end A_Named_Number.Child;
