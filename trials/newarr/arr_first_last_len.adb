procedure Arr_First_Last_Len is
   type My_Int is range 0 .. 100;
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;
   type Arr_M is array (Index) of My_Int;
   type Arr_I is array (Index) of Index;
   type Arr_A is array (1 .. 4, 1 .. 4, 1 .. 4, 1 .. 4) of Integer;
   type Arr_B is array (Integer range 0 .. 5) of Integer;
   type Arr_U is array (Positive range <>) of Integer;
   subtype SU is Arr_U (Index);
   subtype SUN is Arr_U (1 .. 5);
   subtype SUS is Arr_U (Integer range 3 .. 4);
   subtype SSU is SU;
   subtype USU is Arr_U;
   subtype SA is Arr;

   type Arr_Of_Arr is array (5 .. 7) of Arr;
   type Arr_Of_Arrays is array (1 .. 2) of Arr_of_Arr;
   type Arr_Of_SU is array (Integer range <>) of SU;

   L : Integer := 2; U : Integer := 6;
   subtype Dyn_Arr is Arr_U (L .. U);

   type Constrained_Dyn is array (L .. U) of Integer;
   type CD_Arr_Arr is array (L .. U) of Arr;
   type CD_Arr_Arrays is array (L .. U) of Arr_Of_Arr;

   subtype Dyn_Sub is Integer range L .. U;
   subtype Dyn_Arr_S is Arr_U (Dyn_Sub);

   X, Y : Integer;

   procedure PC1 (A : Arr) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1;

   procedure PC1_M (A : Arr_M) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1_M;

   procedure PC1_I (A : Arr_I) is
   begin
      X := A'First;  -- Do_Array_First not called
   end PC1_I;

   procedure PC2 (A : Arr_A) is
      F : Integer;
   begin
      F := A'First (3);  -- Do_Array_First not called
   end PC2;

   procedure PC3 (A : Arr_Of_Arr) is
      F : Integer;
   begin
      F := A'First;  -- Do_Array_First not called
   end PC3;

   procedure PC4 (A : Arr_Of_Arr) is
      F : Integer;
   begin
      F := A (5)'First;  -- Do_Array_First not called
   end PC4;

   procedure PC5 (A : Arr_U; F : out Integer) is
   begin
      F := A'First;  -- Invokes Do_Array_First
   end PC5;

   procedure PC6 (AA : Arr_of_Arr) is
      AAF : Integer;
   begin
      AAF := AA'First;  --  Does not invoke Do_Array_First
      AAF := AA (5)'First;  -- Does not invoke Do_Array_First
   end PC6;

   procedure PC7 (AA : Arr_Of_SU; D1, D2 : out Integer) is
      AAF : Integer;
   begin
      D1 := AA'First;  --  Invoke Do_Array_First
      AAF := SU'First;
      D2 := AA (5)'First;  -- Invokes Do_Array_First
      null;
   end PC7;

   CA : Arr;
   CB : Arr_Of_Arr;

   CU : Arr_U (Index);
   CSUS : SUS;
   CSA : SA;
   CSU : SU;
   CUSU : USU (7 .. 11);
   T : Integer;
   AA : Arr_Of_Arr;
   AAA : Arr_Of_Arrays;
   CDAA : CD_Arr_Arr;
   CDAAA : CD_Arr_Arrays;
   ArrSU : Arr_Of_SU (17 .. 23);
begin
   --  The following uses of attribute 'First are substituted by the
   --  front-end by their constant value;
   X := Arr'First;
   X := Arr_A'First;
   X := Arr_A'First (3);
   X := SU'First;

   X := Dyn_Arr'First;   -- Invokes call to Do_Array_First
   pragma Assert (X = L);
   X := Dyn_Arr'Last;
   pragma Assert (X = U);

   X := Dyn_Arr_S'First; -- Invokes call to Do_Array_First
   pragma Assert (X = L);
   X := Dyn_Arr_S'Last;
   pragma Assert (X = U);

   --  The following uses of attribute 'First on arrays of arrays do not
   --  invoke Do_Array_First.
   X := Arr_Of_Arr'First;
   X := AA (5)'First;
   X := AAA'First;
   X := AAA (1)'First;
   X := AAA (1)(5)'First;
   X := CDAA (1)'First;
   X := CDAAA (1)'First;
   X := CDAAA (1)(5)'First;

   X := Dyn_Sub'First; -- Invokes First_Last_Length
   pragma Assert (X = L);
   X := Dyn_Sub'Last; -- Invokes First_Last_Length
   pragma Assert (X = U);

   X := Constrained_Dyn'First;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = L);
   X := Constrained_Dyn'Last;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = U);
   X := Constrained_Dyn'Length;
   pragma Assert (X = U - L + 1);

   X := CD_Arr_Arr'First;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = L);
   X := CD_Arr_Arr'Last;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = U);
   X := CD_Arr_Arr'Length;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = U - L + 1);

   X := CDAAA'First;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = L);
   X := CDAAA'Last;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = U);
   X := CDAAA'Length;  -- Invokes Do_Array_First_Last_Length
   pragma Assert (X = U - L + 1);

   X := CU'First;
   PC5 (CU, Y);
   pragma Assert (X = Y);

   PC7 (ArrSU, X, Y);
   pragma Assert (X = 17);
   pragma Assert (Y = 1);
   null;
end Arr_First_Last_Len;
