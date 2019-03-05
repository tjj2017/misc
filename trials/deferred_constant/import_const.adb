package body Import_Const is
   procedure Inc (P : in out Priv) is
   begin
      P := P + Imp_Const;
   end Inc;
   
 end Import_Const;
