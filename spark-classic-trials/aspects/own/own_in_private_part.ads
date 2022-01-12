package Own_In_Private_Part is
   V : Integer;
private
   PV : Integer;
   procedure P
     with Classic_Own => (Plain => V);
   function F return Integer
     with Classic_Own => (Plain => V);
end Own_In_Private_Part;
