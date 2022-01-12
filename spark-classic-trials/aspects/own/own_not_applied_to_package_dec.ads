package Own_Not_Applied_To_Package_Dec is
   procedure P
     with Classic_Own => (Plain => V);
   function F return Integer
     with Classic_Own => (Plain => V);
end Own_Not_Applied_To_Package_Dec;
