procedure Access_To_Procedure is
   type Ptr_To_Procedure is access procedure (V : Integer; I : out Integer);

   procedure P (V : Integer; I : out Integer) is
   begin
      I := V;
   end P;

   Ptr_P : Ptr_To_Procedure;

   I : Integer;

begin
   P (3, I);
   pragma Assert (I = 3);

   Ptr_P := P'Access;

   Ptr_P.all (5, I);
   pragma Assert (I = 5);

   Ptr_P (7, I);
   pragma Assert (I = 7);
end Access_To_Procedure;
