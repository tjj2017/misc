package Deferred_Read_Only is
   type Priv is private;
   
   Def_Const : constant Priv;
   
   Dodgy_Var : Integer;
   
   procedure Inc (P : in out Priv);
   
private
   type Priv is range 1 .. 10;
   
   Def_Const : constant Priv := Priv (Dodgy_Var);
end Deferred_Read_Only;

