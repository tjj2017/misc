procedure Array_Assignments is
   type Arr is array (1 .. 10) of integer;
   type U_Arr is array (Positive range  <>) of Integer;
   subtype SU_Arr is U_Arr (1 .. 5);

   function F_Arr return Arr is (11, 12, 13, 14, 15, 16, 17, 18, 19, 20);
   function F_Uarr (L, U : Positive) return U_Arr is (L .. U => 23);
   A : Arr;
   B : constant Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9 ,10);
   C : Arr;
   D : Arr;
   E : Arr;
   F : Arr;

   U1 : U_Arr := (1, 2, 3, 4, 5);
   U2 : U_Arr (1 .. 5);
   U3 : SU_Arr;
   U4 : U_Arr := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   U5 : U_Arr := U4 (1 .. 5);
   U6 : SU_Arr;
   U7 : U_Arr (1 .. 10);
   U8 : U_Arr (7 .. 13);
   U9 : SU_Arr;
   U10 : SU_Arr;
   U11 : U_Arr := (1, 2, 3) & (4, 5, 6);
   U12 : U_Arr := U1 & U4;
begin
   A := B;
   for I in A'Range loop
      pragma Assert (A (I) = B (I));
   end loop;

   A := (10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

   pragma Assert (A (3) = B (8));

   for I in A'Range loop
      pragma Assert (A (A'Last - I + A'First) = I);
   end loop;

   A := (A (10), 2, 3, 4, 5, 6, 7, 8, 9, 10);
   for I in A'Range loop
      pragma Assert (A (I) = I);
   end loop;

   A := F_Arr;
   for I in A'Range loop
      pragma Assert (A (I) = I + 10);
   end loop;

   U2 := U1;
   for I in U2'Range loop
      pragma Assert (U2 (I) = U1 (I));
   end loop;

   U3 := U2;
   for I in U3'Range loop
      pragma Assert (U3 (I) = U1( I));
   end loop;

   for I in U5'Range loop
      pragma Assert (U5 (I) = I);
   end loop;

   U2 := F_Uarr (U2'First, U2'Last);
   for I in U2'Range loop
      pragma Assert (U2 (I) = 23);
   end loop;
   --  The following assertion should fail as all elements are set to 23
   pragma Assert (U2 (3) = 3);

   U2 := (1, 2, 3, 4, 5);
   for I in U2'Range loop
      pragma Assert (U2 (I - U2'First + 1) = I);
   end loop;

   U6 := U4 (1 .. 5);
   for I in U6'Range loop
      pragma Assert (U6 (I) = I);
   end loop;

   U7 := U4 (1 .. 3) & U4 (4 .. 5) & U4 (6 .. 9) & U4 (10);
   for I in U7'Range loop
      pragma Assert (U7 (I) = I);
   end loop;

   C := (1, 2 ,3) & (4, 5) & (6, 7, 8, 9) & 10;
   for I in C'Range loop
      pragma Assert (C (I) = I);
   end loop;

   D (1 .. 3) := (1, 2, 3);
   D (4 .. 5) := (4, 5);
   D (6 .. 10) := (6, 7, 8, 9, 10);

   for I in 1 .. 10 loop
      pragma Assert (D (I) = I);
   end loop;

   E (1 .. 3) := D (1 .. 3);
   E (4 .. 5) := B (4 .. 5);
   E (6 .. E'Last) := F_Arr (6 .. E'Last);

   for I in 1 .. 5 loop
      pragma Assert (E (I) = I);
   end loop;

   for I in 6 .. E'Last loop
      pragma Assert (E (I) = I + 10);
   end loop;

   U8 (7 .. 9) := (1, 2, 3);
   U8 (10 .. 11) := U4 (U4'First + 3 .. U4'First + 4);
   U8 (12 .. 13) := F_Uarr (12, 13);

   for I in U8'First .. U8'Last - 2 loop
      pragma Assert (U8 (I) = I - U8'First + 1);
   end loop;

   pragma Assert (U8 (U8'Last - 1) = 23);
   pragma Assert (U8 (U8'Last) = 23);

   U9 (1 .. 2) := (1, 2);
   U9 (3 .. 4) := U2 (3 .. 4);
   U9 (5 .. 5) := F_Uarr (10, 10);

   for I in 1 .. 4 loop
      pragma Assert (U9 (I) = I);
   end loop;
   pragma Assert (U9 (5) = 23);

   F (1 .. 5) := B (1.. 5);
   F (6 .. 10) := F_Arr (6 .. 10);

   for I in 1 .. 5 loop
      pragma Assert (F (I) = I);
   end loop;

   for I in 6 .. 10 loop
      pragma Assert (F (I) = I + 10);
   end loop;

   U10 := F_Uarr (1, 10) (1 .. 5);

   for I in U10'Range loop
      pragma Assert (U10 (I) = 23);
   end loop;

   C (6 .. 10) := C (1 .. 5);

   for I in 1 .. 5 loop
      pragma Assert (C (I) = I);
      pragma Assert (C (I + 5) = I);
   end loop;

   C (2 .. 4) := C (1 .. 3);
   pragma Assert (C (1) = 1);
   pragma Assert (C (2) = 1);
   pragma Assert (C (3) = 2);
   pragma Assert (C (4) = 3);
   pragma Assert (C (5) = 5);
   for I in 1 .. 5 loop
      pragma Assert (C (I + 5) = I);
   end loop;

   U7 := U4;

   for I in 1 .. 10 loop
      pragma Assert (U7 (I) = I);
   end loop;

   U7 (2 .. 4) := U7 (1 .. 3);
   pragma Assert (U7 (1) = 1);
   pragma Assert (U7 (2) = 1);
   pragma Assert (U7 (3) = 2);
   pragma Assert (U7 (4) = 3);
   pragma Assert (U7 (5) = 5);
   for I in 6 .. 10 loop
      pragma Assert (U7 (I) = I);
   end loop;

   D := (D (10), D (9), D (8), D (7), D (6), D (5), D (4), D (3), D (2), D (1));

   for I in D'Range loop
      pragma Assert (D (I) = 10 - I + 1);
   end loop;

   D := D (10) & D (9) & D (8) & D (7) & D (6) & D (5) & D (4) & D (3) & D (2) & D (1);

   for I in D'Range loop
      pragma Assert (D (I) = I);
   end loop;

   U9 := U1;
   U9 := (U9 (5), U9 (4), U9 (3), U9 (2), U9 (1));

   for I in U9'Range loop
      pragma Assert (U9 (I) = 5 - I + 1);
   end loop;

   U9 := U9 (5) & U9 (4) & U9 (3) & U9 (2) & U9 (1);

   for I in U9'Range loop
      pragma Assert (U9 (I) = I);
   end loop;

   for I in U11'Range loop
      pragma Assert (U11 (I) = I - U11'First + 1);
   end loop;

   pragma Assert (U12 (U12'First) = 1);
end Array_Assignments;
