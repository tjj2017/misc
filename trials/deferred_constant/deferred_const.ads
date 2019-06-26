pragma SPARK_MODE;
package Deferred_Const is
   type Priv is private;
   
   Def_Const : constant Priv;
   Def_Const_Plus_1 : constant Priv;
   
   procedure Inc (P : in out Priv);
   
private
   type Priv is range 1 .. 10;
   
   Def_Const : constant Priv := 1;
   Def_Const_Plus_1 := Def_Const + 1;
end Deferred_Const;

