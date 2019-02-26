package Deferred_Const is
   type Priv is private;
   
   Def_Const : constant Priv;
   
   function Where (N : Natural) return Priv;
   
private
   type Priv is (Empty, Half_Full, Full);
   
   Def_Const : constant Priv := Empty;
end Deferred_Const;

