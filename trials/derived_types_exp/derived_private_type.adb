package body Derived_Private_Type is
   procedure Nix is
   begin
      pragma Assert (P (D_P (P_Const)) = P_Const);
      null;
   end;
end Derived_Private_Type;
