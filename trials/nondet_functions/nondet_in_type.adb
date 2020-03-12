with Types; use Types;
with Nondet;
procedure Nondet_In_Type (V : in out My_Sub) is
begin
   V := Nondet;
   pragma Assume (V in My_Sub);
end Nondet_In_Type;
