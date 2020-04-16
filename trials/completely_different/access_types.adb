procedure Access_Types is
   type A_Int is access all Integer;

   I : aliased Integer := 3;

   Acc : A_Int;

   type Count is range 0 .. 10;
   subtype Index is Count range 1 .. Count'Last;
   type Vector is array (Index) of Count;
   type Header is record
      I : Integer;
      Counter : Count;
   end record;

   type Buffer_1 is record
      Head : Header;
      Vec  : Vector;
   end record;

   type Buffer_1_Pointer is access all Buffer_1;

   B_1 : aliased Buffer_1;
   B_1_P : Buffer_1_Pointer;
   B_1_P_2 : Buffer_1_Pointer;

begin
   Acc := I'Access;
   pragma Assert (Acc.all = 3);

   B_1_P := B_1'Access;

   B_1_P.Head.I := I;
   pragma Assert (B_1.Head.I = 3);

   B_1_P_2 := B_1_P;
end Access_Types;
