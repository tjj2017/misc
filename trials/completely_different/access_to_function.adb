procedure Access_To_Function is
   type Ptr_To_Function is access function (I : Integer) return Integer;

   function F (V : Integer) return Integer is (V);

   Ptr_F : Ptr_To_Function;

   I : Integer;

begin
   I := F (3);
   pragma Assert (I = 3);

   Ptr_F := F'Access;

   I := Ptr_F.all (5);
   pragma Assert (I = 5);

   I := Ptr_F (7);
   pragma Assert (I = 7);
end Access_To_Function;
