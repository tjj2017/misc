with My_Model; use My_Model;
procedure Irep_In_Type is

--     K : Integer := 0;
--     S : Small := 19;
--     L : Smaller := 3;
--     E : My_Enum := two;
--     P : Sub_Enum := three;
--  --   R : My_Mod := 16;
--     Fixed_Array_Const : constant My_Fixed_Array := (others => 7);
--     U_Array_Const : constant My_Unconstrained_Array (1 .. 3) := (others => 11);
--     AA_Array_Const : constant Array_Of_Arrays := (others => (others => 13));
--
   Fixed_Array : My_Fixed_Array := (others => 7);
--     U_Array : My_Unconstrained_Array (1 ..3) := (others => 11);
--     AA_Array : Array_Of_Arrays := (others => (others => 13));
--
--     A_Rec_Const : constant R := (1, 2);
--     A_Rec : R := A_Rec_Const;

begin
   for i in Fixed_Array'Range loop
      Small_In_Type (Fixed_Array (I));
   end loop;

   for i in Fixed_Array'Range loop
      pragma Assert (Fixed_Array (I) = 7);
   end loop;

   for i in Fixed_Array'Range loop
      pragma Assert (Fixed_Array (I) >= Small'First and
                       Fixed_Array (I) <= Small'Last);
   end loop;

--     Int_In_Type (K);
--     pragma Assert (K = 0);
--
--     Small_In_Type (S);
--     Small_In_Type (L);
--     My_Enum_In_Type (E);
--     My_Enum_In_Type (P);
--  --   My_Mod_In_Type (R);
--
--     pragma Assert (S = 19);
--     pragma Assert (S >= Small'First and S <= Small'Last);
--
--     pragma Assert (L = 3);
--     pragma Assert (L >= 1 and L <= 5);
--
--     pragma Assert (E = two);
--     pragma Assert (E >= one and E <= five);
--
--     pragma Assert (P = three);
--     pragma Assert (P in Sub_Enum);
--
--  --   pragma Assert (Fixed_Array = Fixed_Array_Const);
--     for I in Fixed_Array'Range loop
--        pragma Assert (Fixed_Array (I) = Fixed_Array_Const (I));
--     end loop;
--
--  --   pragma Assert (U_Array = U_Array_Const);
--     for I in U_Array'Range loop
--        pragma Assert (U_Array (I) = U_Array_Const (I));
--     end loop;
--
--  --   pragma Assert (AA_Array = AA_Array_Const);
--     for I in AA_Array'Range loop
--        for J in AA_Array (I)'Range loop
--           pragma Assert (AA_Array (I) (J) = AA_Array_Const (I) (J));
--        end loop;
--     end loop;
--
--  --   pragma Assert (AA_Array = AA_Array_Const);
--     for I in AA_Array'Range loop
--        for J in AA_Array (I)'Range loop
--           pragma Assert (AA_Array (I) (J) >= Small'First and
--                          AA_Array (I) (J) <= Small'Last);
--        end loop;
--     end loop;

   My_Fixed_Array_In_Type (Fixed_Array);
--   My_Unconstrained_Array_In_Type (U_Array);
--   Array_Of_Arrays_In_Type (AA_Array);

--   pragma Assert (Fixed_Array = Fixed_Array_Const);
--     for I in Fixed_Array'Range loop
--        pragma Assert (Fixed_Array (I) = Fixed_Array_Const (I));
--     end loop;
--
   for I in Fixed_Array'Range loop
      pragma Assert (Fixed_Array (I) >= Small'First and
                       Fixed_Array (I) <= Small'Last);
   end loop;
--
--     --   pragma Assert (U_Array = U_Array_Const);
--     for I in U_Array'Range loop
--        pragma Assert (U_Array (I) = U_Array_Const (I));
--     end loop;
--
--     for I in U_Array'Range loop
--        pragma Assert (U_Array (I) >= Small'First and
--                         U_Array (I) <= Small'Last);
--     end loop;
--
--  --   pragma Assert (AA_Array = AA_Array_Const);
--     for I in AA_Array'Range loop
--        for J in AA_Array (I)'Range loop
--           pragma Assert (AA_Array (I) (J) = AA_Array_Const (I) (J));
--        end loop;
--     end loop;
--
--
--     In_Type_R (A_Rec);
--
--     pragma Assert (A_Rec = A_Rec_Const);
--     pragma Assert (A_Rec.S >= Small'First and A_Rec.S <= Small'Last);
--     pragma Assert (A_Rec.T >= Small'First and A_Rec.T <= Small'Last);

end Irep_In_Type;
