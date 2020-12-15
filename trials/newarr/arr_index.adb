procedure Arr_Index is
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;
   type Arr_2 is array (5 .. 14) of Integer;
   type Arr_Arr is array (Index) of Arr;

   type Arr_U is array (Integer range  <>) of Integer;
   type Arr_P is access all Arr;

   subtype S1 is Integer range 1 .. 3;
   subtype S2 is Integer range 1 .. 2;
   subtype S3 is Integer range 1 .. 4;
   type Multi_D is array (S1, S2, S3) of Integer;
   type Multi_U is array (S1 range <>, S2 range <>, S3 range <>) of Integer;

   A : aliased Arr;
   I : Integer;
   X : Integer;

   AA : Arr_Arr;
   AP : Arr_P := A'Access;

   MA : Multi_D;
   C : Natural;

   MU : Multi_U (S1, S2, S3);
begin
   I := 6;
   A (I) := 23;
   X := A (I);
   pragma Assert (X = 23);

   AA (I) (I) := 17;
   X := AA (I) (I);
   pragma Assert (X = 17);

   X := A (4 .. 7) (I);
   pragma Assert (X = 23);

   X := A (4 .. 7) (5 .. 7) (I);
   pragma Assert (X = 23);

   X := A (4 .. 7) (5 .. 7) (6 .. 6) (I);
   pragma Assert (X = 23);

   A (3 .. 5) (4) := 19;
   X := A (A'First .. A'Last) (4);
   pragma Assert (X = 19);

   A (2 .. 4) (3) := A (A'First .. 7) (I);
   X := A (A'First .. A'Last) (3);
   pragma Assert (X = 23);

   declare
      subtype SU is Arr_U (A'First .. X);
      ASU : SU;
   begin
      ASU (ASU'First .. A'Last) (I) := A (2 .. 6) (I);
      pragma Assert (ASU (3 .. 7) (I) = 23);
   end;

   declare
      type CA is array (A'First .. X) of Integer;
      ACA : CA;
   begin
      ACA (ACA'First .. A'Last) (I) := A (2 .. 6) (I);
      pragma Assert (ACA (3 .. 7) (I) = 23);
   end;

   pragma Assert (AP (I) = 23);
   pragma Assert (AP (A'First .. I) (4) = 19);
   pragma Assert (AP (AP'First .. I) (4) = 19);
   AP (AP'First .. 6) (5) := AA (AA'First .. I + 3) (I) (Arr'First .. Arr'Last) (I);
   pragma Assert (AP (3 .. AP'Last) (5) = 17);

--     --  Type conversion of a slice is not currently supported.
--     X := Arr_2 (A (Arr'First .. 7)) (8);
--     pragma Assert (X = 19);

   X := Arr_2 (A) (8);
   pragma Assert (X = 19);

   C := 0;
   for I in S1 loop
      for J in S2 loop
         for K in S3 loop
            C := C + 1;
            MA (I, J, K) := C;
         end loop;
      end loop;
   end loop;

   C := 0;
   for I in S1 loop
      for J in S2 loop
         for K in S3 loop
            C := C + 1;
            pragma Assert (MA (I, J, K) = C);
         end loop;
      end loop;
   end loop;

   C := 0;
   for I in S1 loop
      for J in S2 loop
         for K in S3 loop
            C := C + 1;
            MU (I, J, K) := C;
         end loop;
      end loop;
   end loop;

   C := 0;
   for I in S1 loop
      for J in S2 loop
         for K in S3 loop
            C := C + 1;
            pragma Assert (MU (I, J, K) = C);
         end loop;
      end loop;
   end loop;

end Arr_Index;
