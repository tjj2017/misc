procedure Dynamic is
   type Arr_U is array (Integer range <>) of Integer;
   Low : Integer;
   High : Integer;
begin
   Low := 1;
   High := 10;
   declare
      subtype A is Arr_U (Low .. High);
      VA : A;
   begin
      VA (1) := 10;
      VA (2) := 2;
      VA (VA'Last) := VA (VA'First) * VA (2);
      pragma Assert (VA (10) = 20);
   end;
   declare
      subtype US is Arr_U;
      subtype CS is US (Low .. High);
      VB : CS;
   begin
      VB (1) := 10;
      VB (2) := 2;
      VB (VB'Last) := VB (VB'First) * VB (2);
      pragma Assert (VB (10) = 20);
   end;
   declare
      type CT is array (Low .. High) of Integer;
      VC : CT;
   begin
      VC (1) := 10;
      VC (2) := 2;
      VC (VC'Last) := VC (VC'First) * VC (2);
      pragma Assert (VC (10) = 20);
   end;
end Dynamic;
