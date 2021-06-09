procedure Try_Subtype_Bounds is
   subtype Index is Integer range 1 .. 10;
   type Arr_U is array (Positive range <>) of Integer;
   subtype SU is Arr_U (Index);
   procedure P1 (A : Arr_U) is
   begin
      pragma Assert (A'First = 1);
   end P1;

   VL, VH : Integer;

   AU : SU;
begin
   VL := 1;
   VH := 8;
   declare
      type Dyn_Arr is array (VL .. 10) of Integer;
      AD : Dyn_Arr;
   begin
      pragma Assert (AD'First = 1);
   end;

   P1 (AU);
end Try_Subtype_Bounds;
