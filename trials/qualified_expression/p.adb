package body P is
   
   procedure Qualified_Expression (R1, R2 : out A_Pair) is
      
      subtype Small is Integer range 0 .. 10;
      
      S1, S2 : Small; 
      V1, V2 : Integer;
   
      function F (X : Integer) return Integer is
      begin 
	 return (X * 15); 
      end F;

   begin 
      V1 := Small'(5 + 3); 
      V2 := Small'(V1 + 17);
      
      S1 := Small'(V1 + 2); 
      S2 := Small'(F (S1));
      
      R1 := A_Pair'(S1, S2); 
      R2 := A_Pair'(Small'(V1), Small'(V2 + 2)); 
   end Qualified_Expression;
end P;
