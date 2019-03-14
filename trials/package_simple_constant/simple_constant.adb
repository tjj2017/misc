package body Simple_Constant is
   --  function C return Integer is 
   --  begin 
   --     return Integer'Last;
   --  end C;

   procedure P is --  (X : out Integer) is
      X : Integer := 1;
      --      C : Integer := Integer'Last;
   begin
     X := X + C;
   end P;
   
end Simple_Constant;
   
