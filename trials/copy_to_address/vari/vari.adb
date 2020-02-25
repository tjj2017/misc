with Text_IO; use Text_IO;
procedure Vari is
   subtype Count is Integer range 0 .. 10;
   subtype Index is Count range Count'First .. Count'last - 1;
   type AF is array (Index) of Integer;
   type F is record
      C : Count;
      D : AF;
   end record;

   type AV is array (Index range  <>) of Integer;
   type V (N : Index) is record
      C : Count;
      D : AV (Count'First .. N);
   end record;

   subtype VF is V (Count'Last - 1);

   V_F : F := (0, (others => 0));
   V_V : VF := (Count'Last - 1, 0, (others => 0));
begin
   Put_Line ("V_F'Size = " & Integer'Image (V_F'Size));
   Put_Line ("V_V'Size = " & Integer'Image (V_V'Size));
end Vari;
