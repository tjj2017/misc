with My_Model; use My_Model;
procedure Irep_In_Type is

   --     K : Integer := 0;
   S : Small := 19;
--     L : Smaller := 11;
--     E : My_Enum := two;
--     P : Sub_Enum := three;
--     R : My_Mod := 16;
   Fixed_Array_Const : constant My_Fixed_Array := (others => 7);
   U_Array_Const : constant My_Unconstrained_Array (1 .. 3) := (others => 11);
   AA_Array_Const : constant Array_Of_Arrays := (others => (others => 13));

   Fixed_Array : My_Fixed_Array := (others => 7);
   U_Array : My_Unconstrained_Array (1 ..3) := (others => 11);
   AA_Array : Array_Of_Arrays := (others => (others => 13));

   A_Rec_Const : constant R := (1, 2);
   A_Rec : R := A_Rec_Const;

begin
   --     Int_In_Type (K);
   Small_In_Type (S);
--     Small_In_Type (L);
--     My_Enum_In_Type (E);
--     My_Enum_In_Type (P);
--     My_Mod_In_Type (R);

   pragma Assert (Fixed_Array = Fixed_Array_Const);
   pragma Assert (U_Array = U_Array_Const);
   pragma Assert (AA_Array = AA_Array_Const);

   My_Fixed_Array_In_Type (Fixed_Array);
   My_Unconstrained_Array_In_Type (U_Array);
   Array_Of_Arrays_In_Type (AA_Array);

   pragma Assert (Fixed_Array = Fixed_Array_Const);
   pragma Assert (U_Array = U_Array_Const);
   pragma Assert (AA_Array = AA_Array_Const);

--   pragma Assert (A_Rec = A_Rec_Const);

   In_Type_R (A_Rec);

--   pragma Assert (A_Rec = A_Rec_Const);

end Irep_In_Type;
