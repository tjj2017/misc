package In_Private is
--     pragma Classic_Own ((Plain =V, Plain => P));
   V : Integer;
private
   pragma Classic_Own ((Plain =V, Plain => P));
   P : Integer;
end In_Private;
