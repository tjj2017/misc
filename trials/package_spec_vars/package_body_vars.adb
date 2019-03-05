package body Package_Body_Vars is
   V : Integer := Integer'Last;
   procedure P is
      X : Integer := 1;
   begin
      X := X + V;
   end P;
end Package_Body_Vars;

