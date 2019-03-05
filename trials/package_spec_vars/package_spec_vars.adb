package body Package_Spec_Vars is
   procedure P is
      X : Integer := Integer'Last;
   begin
      pragma Assert (X + V <= Integer'Last);
      X := X + V;
   end P;
   
end Package_Spec_Vars;

