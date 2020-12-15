procedure Arr_Agg is
   subtype Index is Integer range 1 .. 10;
   subtype Sub_Index is Index range 9 .. 9;
   type Arr is array (Index) of Integer;

   type U_Arr is Array (Integer range <>) of Integer;

   function Fun return Arr is
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

   function Fun2 (L, U : Index; V : Integer) return Arr is (L .. U => V);

--     function Fun3 (L, U : Index; V : Integer) return U_Arr is (L .. U => V);


   Eight : constant Integer := 8;
   A : Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   B : Arr := (1 .. 3 => 3, 4 | 6 => 5, Eight => 11, Sub_Index => 13,
               others => 7);
   C : Arr := (1, 2, 3, others => 11);
   D : Arr := (4, 5, 6, others => <>);

      J : U_Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);  -- Does not work
   subtype UA_Sub is U_Arr (1 .. 10);
   subtype UA_Small is U_Arr (1 .. 4);
   subtype UA_Small_Off is U_Arr (7 .. 10);
   K : UA_Sub := (1, 2 ,3, 4, 5, 6, others => 10);

   L : UA_Sub := (2 .. 5 => 7, Eight => 8, others => 91);

   Low, High : Integer;

   P : Arr := A;

   USmall : UA_Small := K (1 ..4);
   VSmall : UA_Small := K (2 .. 5);
   WSmall : UA_Small_Off := K (3 .. 6);
   Z : Arr := (1, 2, 3, 4, 5) & (6, 7, 8, 9, 10);
   AF : Arr := Fun;
   AF2 : Arr := Fun2 (Index'First, Index'Last, 23);
begin
   Low := 1;
   High := 10;
   declare
      E : Arr := (Low .. High => 3);
      type S is array (Low .. High) of Integer;
      function Fun4 return S is
        (1, 2, 3, 4, 4, 6, 7, 8, 9, 10);
      function Fun5 (L, U, V : Integer) return S is (L .. U => V);

      F : S := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      G : S := (3, 4, 5, others => 17);
      H : Arr := (1 .. 3 => 19, 5 => 23, 7 .. 8 => 29, others => 31);

      subtype UAD is U_Arr (Low .. High);
      M : UAD := (1, 2, 3, 4, 5, 6, 7, others => 10);
      N : UAD := (others => 43);
      O : UAD := (2 .. 6 => 37, Eight => Eight, 10 => 11, others => 47);

      Q : UA_Sub := N;
      R : UAD := K;

      SS : S := F;
      TU : UAD := O;
      XSmall : UA_Small := K (Low + 1 .. High - 5);

      Y : S := F (Low .. High);
      SF : S := Fun4;
      SF2 : S := Fun5 (Low, High, 29);
   begin
      for I in E'Range loop
         pragma Assert (E (I) = 3);
      end loop;

      for I in F'Range loop
         pragma Assert (F (I) = I);
      end loop;

      pragma Assert (G (1) = 3);
      pragma Assert (G (2) = 4);
      pragma Assert (G (3) = 5);

      for I in 4 .. G'Last loop
         pragma Assert (G (I) = 17);
      end loop;

      pragma Assert (H (5) = 23);
      pragma Assert (H (7) = 29);
      pragma Assert (H (8) = 29);
      pragma Assert (H (4) = 31);
      pragma Assert (H (9) = 31);
      pragma Assert (H (10) = 31);

      for I in 1 .. 3 loop
         pragma Assert (H (I) = 19);
      end loop;

      for I in M'Range loop
         if I <= 7 then
            pragma Assert (M (I) = I);
         else
            pragma Assert (M (I) = 10);
         end if;
      end loop;

      for I in N'Range loop
         pragma Assert (N (I) = 43);
      end loop;

      pragma Assert (O (1) = 47);
      for I in 2 .. 6 loop
         pragma Assert (O (I) = 37);
      end loop;
      pragma Assert (O (7) = 47);
      pragma Assert (O (8) = 8);
      pragma Assert (O (9) = 47);
      pragma Assert (O (10) = 11);

      for I in Q'Range loop
         pragma Assert (Q (I) = 43);
      end loop;

      for I in R'Range loop
         if I <= 6 then
            pragma Assert (R (I) = I);
         else
            pragma Assert (R (I) = 10);
         end if;
      end loop;

      for I in SS'Range loop
         pragma Assert (SS (I) = I);
      end loop;

      pragma Assert (TU (1) = 47);
      for I in 2 .. 6 loop
         pragma Assert (TU (I) = 37);
      end loop;
      pragma Assert (TU (7) = 47);
      pragma Assert (TU (8) = 8);
      pragma Assert (TU (9) = 47);
      pragma Assert (TU (10) = 11);

      for I in XSmall'Range loop
         pragma Assert (XSmall (I) = I + 1);
      end loop;

      for I in Y'Range loop
         pragma Assert (Y (I) = I);
      end loop;

      pragma Assert (Fun4'First = 1);
      pragma Assert (Fun4'Last = High);
      pragma Assert (SF'First = 1);
      pragma Assert (SF'Last = High);
--        pragma Assert (Fun3 (Low, High, 101)'First = 1);
      for I in SF'Range loop
         pragma Assert (SF (I) = I);
      end loop;

      for I in SF2'Range loop
         pragma Assert (SF2 (I) = 29);
      end loop;

     null;
   end;
   for I in A'Range loop
      pragma Assert (A (I) = I);
   end loop;

   for I in C'Range loop
      pragma Assert (C (I) = (if I <= 3 then I else 11));
   end loop;

   pragma Assert (D (1) = 4);
   pragma Assert (D (2) = 5);
   pragma Assert (D (3) = 6);
   pragma Assert (D (4) = 7);

   pragma Assert (B (1) = 3);
   pragma Assert (B (2) = 3);
   pragma Assert (B (3) = 3);

   pragma Assert (B (4) = 5);
   pragma Assert (B (6) = 5);

   pragma Assert (B (5) = 7);
   pragma Assert (B (7) = 7);
   pragma Assert (B (8) = 11);
   pragma Assert (B (9) = 13);
   pragma Assert (B (10) = 7);

--     for I in 1 .. 10 loop
--        pragma Assert (J (I) = I);
--     end loop;
--
   for I in K'Range loop
      if I <= 6 then
         pragma Assert (K (I) = I);
      else
         pragma Assert (K (I) = 10);
      end if;
   end loop;

   pragma Assert (L (1) = 91);
   for I in 2 ..5 loop
      pragma Assert (L (I) = 7);
   end loop;
   pragma Assert (L (6) = 91);
   pragma Assert (L (7) = 91);
   pragma Assert (L (8) = 8);
   pragma Assert (L (9) = 91);
   pragma Assert (L (10) = 91);

   for I in P'Range loop
      pragma Assert (P (I) = I);
   end loop;

   for I in USmall'Range loop
      pragma Assert (USmall (I) = I);
   end loop;

   for I in USmall'Range loop
      pragma Assert (VSmall (I) = I + 1);
   end loop;

   for I in WSmall'Range loop
      pragma Assert (WSmall (I) = I - WSmall'First + K'First + 2);
   end loop;

   for I in Z'Range loop
      pragma Assert (Z (I) = I);
   end loop;

   for I in AF'Range loop
      pragma Assert (AF (I) = I);
   end loop;

   for I in AF2'Range loop
      pragma Assert (AF2 (I) = 23);
   end loop;

   null;
end Arr_Agg;
