package body Deferred_Read_Only is
   procedure Inc (P : in out Priv) is
   begin
      P := P + Def_Const;
   end Inc;
   
end Deferred_Read_Only;
