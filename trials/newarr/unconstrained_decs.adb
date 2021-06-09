procedure Unconstrained_Decs is
   type U_Array is array (Integer range <>) of Integer;
   subtype U_C_Array is U_Array (1 .. 4);

   function F return U_C_Array is (4, 3, 2, 1);

   function UF return U_Array is (4, 3, 2, 1);

   A : U_Array := (1, 2, 3);
   A1 : U_Array := (4, 5, 6);
   B : U_Array := A;
   B1 : U_Array := A1;
   C : U_Array := F;
   D : U_Array := UF;
   E : U_C_Array := (1, 2, 3, 4);
   EB : U_C_Array := (4, 3, 2, 1);
   P : U_Array := (1, 2, 3, 4) & (5, 6, 7, 8, 9, 10);

   AB : U_Array := B (B'First .. B'Last - 1);
   EE  : U_Array := E (1 .. 3);
   A_And_A : U_Array := (1, 2, 3) & (4, 5, 6);
   A_And_B : U_Array := A & B;

   U4 : U_Array := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   U5 : U_C_Array := U4 (U4'First .. U4'First + 3);

   L, U : Integer;
begin
   pragma Assert (A'Length = 3);
   pragma Assert (B'Length = 3);
   pragma Assert (A'First = Integer'First);
   pragma Assert (B'First = Integer'First);
   pragma Assert (A'Last = Integer'First + A'Length - 1);
   pragma Assert (B'Last = Integer'First + B'Length - 1);

   pragma Assert (P'First = Integer'First);
   pragma Assert (P'Length = 10);
   pragma Assert (P'Last = P'First + P'Length - 1);

   pragma Assert (A_And_B'First = Integer'First);
   pragma Assert (A_And_B'Length = 6);
   pragma Assert (A_And_B'First = Integer'First);
   pragma Assert (A_And_B'Last = Integer'First + A_And_B'Length - 1);
   pragma Assert (A_And_B (A_And_B'First) = 1);

   L := 1;
   U := 4;

   declare
      V_Obj : U_Array (L .. U) := (1, 2, 3, 4, 5);
      V_Concat : U_Array := V_Obj & V_Obj;
      subtype V_Array is U_array (L .. U);
      function VF return V_Array is (others => 23);
      function VUF (Low, Up : Integer) return U_Array is (Low .. Up => 37);
      VA : V_Array := E;
      VB : U_Array := VUF (L, U);
   begin
      for I in VA'Range loop
         pragma Assert (VA (I) = I);
      end loop;
      null;
   end;

   for I in A'Range loop
      pragma Assert (A (I) = I - A'First + 1);
   end loop;

   for I in B'Range loop
      pragma Assert (B (I) = I - B'First + 1);
   end loop;

   for I in AB'Range loop
      pragma Assert (AB (I) = B (I));
   end loop;

   for I in E'Range loop
      pragma Assert (E (I) = EB (EB'Last - I + 1));
   end loop;


   for I in EE'Range loop
      EE (I) := I;
   end loop;

   for I in EE'Range loop
      pragma Assert (EE (I) = I);
   end loop;

   for I in A_And_A'Range loop
      pragma Assert (A_And_A (I) = I - A_and_A'First + 1);
   end loop;

   for I in A'Range loop
      pragma Assert (A (I) = I - A'First + 1);
   end loop;

   for I in B'Range loop
      pragma Assert (B (I) = I - B'First + 1);
   end loop;

   for I in P'Range loop
      pragma Assert (P (I) = I - P'First + 1);
   end loop;

   pragma Assert (A_And_B (A_And_B'First + 1) = 2);
   pragma Assert (A_And_B (A_And_B'First + 2) = 3);

   for I in A_And_B'First .. A_And_B'First + 2 loop
      pragma Assert (A_And_B (I) = I - A_And_B'First + 1);
   end loop;


   for I in A_And_B'Range loop
      if I - A_And_B'First + 1 <= 3 then
         pragma Assert (A_And_B (I) = I - A_And_B'First + 1);
      else
         pragma Assert (A_And_B (I) = I - A_And_B'First + 1 - 3);
      end if;
   end loop;

   for I in 1 .. 3 loop
      pragma Assert (A_And_B (A_And_B'First + I - 1) = I);
   end loop;
   for I in 1 .. 3 loop
      pragma Assert (A_And_B (A_And_B'First + I - 1 + 3) = I);
   end loop;

   for I in U4'Range loop
      pragma Assert (U4 (I) = I - U4'First + 1);
   end loop;

   for I in U5'Range loop
      pragma Assert (U5 (I) = I);
   end loop;

   null;
end Unconstrained_Decs;
