procedure Array_Param is
   subtype Index is Integer range 1 .. 10;
   type Arr is array (Index) of Integer;
   type Arr_U is array (Integer range <>) of Integer;
   subtype SU is Arr_U (3 .. 4);

   type Multi_Array is array (1 .. 2, 1 .. 3, 1 .. 4) of Integer;
   type MA_U is array
     (Integer range <>, Integer range <>, Integer range <>) of Integer;
   subtype SMA_U is MA_U (1..2, 1..3, 1..4);

   type Arr_Of_Arr is array (1 .. 2) of Arr;
   type Arr_Of_Arr_U is array (Integer range <>) of Arr;
   subtype SAA_U is Arr_Of_Arr_U (3 .. 4);

   procedure Constrained_Array_Param (A : Arr;
                                      F, L, Len : out Integer) is
   begin
      F := A'First;
      L := A'Last;
      Len := A'Length;
   end Constrained_Array_Param;

   procedure Unconstrained_Array_Param (A : Arr_U;
                                        F, L, Len : out Integer) is
   begin
      F := A'First;
      L := A'Last;
      Len := A'Length;
   end Unconstrained_Array_Param;

   procedure Multi_Array_Param (AA : Multi_Array;
                                Dim : Integer;
                                F, L, Len : out Integer) is
   begin
      case Dim is
      when 1 =>
         F := AA'First (1);
         L := AA'Last (1);
         Len := AA'Length (1);
      when 2 =>
         F := AA'First (2);
         L := AA'Last (2);
         Len := AA'Length (2);
      when 3 =>
         F := AA'First (3);
         L := AA'Last (3);
         Len := AA'Length (3);
      when others =>
         null;
      end case;
   end Multi_Array_Param;

   procedure Unconstrained_Multi_Array_Param (AA : MA_U;
                                Dim : Integer;
                                F, L, Len : out Integer) is
   begin
      case Dim is
      when 1 =>
         F := AA'First (1);
         L := AA'Last (1);
         Len := AA'Length (1);
      when 2 =>
         F := AA'First (2);
         L := AA'Last (2);
         Len := AA'Length (2);
      when 3 =>
         F := AA'First (3);
         L := AA'Last (3);
         Len := AA'Length (3);
      when others =>
         null;
      end case;
   end Unconstrained_Multi_Array_Param;

   procedure Arr_Arr_Param (A_A : Arr_Of_Arr;
                            Which : Integer;
                            F, L, Len : out Integer) is
   begin
      case Which is
         when 1 =>
            F := A_A'First;
            L := A_A'Last;
            Len := A_A'Length;
         when 2 =>
            F := A_A (1)'First;
            L := A_A (2)'Last;
            Len := A_A (1)'Length;
         when others =>
            null;
      end case;
   end Arr_Arr_Param;

   procedure Unconstrained_Arr_Arr_Param (A_A : Arr_Of_Arr_U;
                                          Which : Integer;
                                          F, L, Len : out Integer) is
   begin
      case Which is
         when 1 =>
            F := A_A'First;
            L := A_A'Last;
            Len := A_A'Length;
         when 2 =>
            F := A_A (1)'First;
            L := A_A (2)'Last;
            Len := A_A (1)'Length;
         when others =>
            null;
      end case;
   end Unconstrained_Arr_Arr_Param;

   AV : Arr;
   SUV : SU;
   MV  : Multi_Array;
   UMV : SMA_U;
   AAV : Arr_Of_Arr;
   SUAAV : SAA_U;

   XF, XL, XLen  : Integer;

begin
   Constrained_Array_Param (AV, XF, XL, XLen);
   pragma Assert (XF = Index'First);
   pragma Assert (XL = Index'Last);
   pragma Assert (XLen = Arr'Length);

   Unconstrained_Array_Param (SUV, XF, XL, XLen);
   pragma assert (XF = SU'First);
   pragma Assert (XL = SU'Last);
   pragma Assert (XLen = SU'Length);

   Multi_Array_Param (MV, 1, XF, XL, XLen);
   pragma assert (XF = Multi_Array'First);
   pragma Assert (XL = Multi_Array'Last);
   pragma Assert (XLen = Multi_Array'Length);

   Multi_Array_Param (MV, 2, XF, XL, XLen);
   pragma assert (XF = Multi_Array'First (2));
   pragma Assert (XL = Multi_Array'Last (2));
   pragma Assert (XLen = Multi_Array'Length (2));

   Multi_Array_Param (MV, 3, XF, XL, XLen);
   pragma assert (XF = Multi_Array'First (3));
   pragma Assert (XL = Multi_Array'Last (3));
   pragma Assert (XLen = Multi_Array'Length (3));
   Multi_Array_Param (MV, 1, XF, XL, XLen);
   pragma assert (XF = Multi_Array'First);
   pragma Assert (XL = Multi_Array'Last);
   pragma Assert (XLen = Multi_Array'Length);

   Unconstrained_Multi_Array_Param (UMV, 1, XF, XL, XLen);
   pragma assert (XF = UMV'First);
   pragma Assert (XL = UMV'Last);
   pragma Assert (XLen = UMV'Length);

   Unconstrained_Multi_Array_Param (UMV, 2, XF, XL, XLen);
   pragma assert (XF = UMV'First (2));
   pragma Assert (XL = UMV'Last (2));
   pragma Assert (XLen = UMV'Length (2));

   Unconstrained_Multi_Array_Param (UMV, 3, XF, XL, XLen);
   pragma assert (XF = UMV'First (3));
   pragma Assert (XL = UMV'Last (3));
   pragma Assert (XLen = UMV'Length (3));

   Arr_Arr_Param (AAV, 1, XF, XL, XLen);
   pragma Assert (XF = Arr_Of_Arr'First);
   pragma Assert (XL = Arr_Of_Arr'Last);
   pragma Assert (XLen = Arr_Of_Arr'Length);

   Arr_Arr_Param (AAV, 2, XF, XL, XLen);
   pragma Assert (XF = Arr'First);
   pragma Assert (XL = Arr'Last);
   pragma Assert (XLen = Arr'Length);

   Unconstrained_Arr_Arr_Param (SUAAV, 1, XF, XL, XLen);
   pragma Assert (XF = SAA_U'First);
   pragma Assert (XL = SAA_U'Last);
   pragma Assert (XLen = SAA_U'Length);

   Unconstrained_Arr_Arr_Param (SUAAV, 2, XF, XL, XLen);
   pragma Assert (XF = Arr'First);
   pragma Assert (XL = Arr'Last);
   pragma Assert (XLen = Arr'Length);
end Array_Param;
