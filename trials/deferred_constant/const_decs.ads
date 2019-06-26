pragma SPARK_MODE;
package Const_Decs is
   Var : Integer := 3;
   
   Static_Const : constant Integer := 42;
   
   SC1 : constant Integer := Static_Const + 1;
   
   Read_Only_Var : constant Integer := SC1 + Var;
   
   procedure Inc (P : in out Integer);
   
end Const_Decs;

