package P is
    type A_Pair is record 
       A, B : Integer; 
    end record;
   procedure Qualified_Expression (R1, R2 : out A_Pair);
end P;

