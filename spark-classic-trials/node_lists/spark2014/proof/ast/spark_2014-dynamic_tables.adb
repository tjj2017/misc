---------------------  SPARK_2014.Dynamic_Tables ----------------------------
--  This package body has to be excluded from SPARK 2014 analysis as,      --
--  with the 2021 version of SPARK being used, SPARK_Mode => Off does not  --
--  entirely prevent analysis of the body and SPARK 2014 does not resolve  --
--  the state refinement properly.  In this package body abstract state    --
--  The_Table is refined to T, but the Pre and Post aspects in the         --
--  declaration that call other functions of the package with the Global   --
--  The_Table, then an error message is generated stating                  --
--  "Dynamic_Tables.The_Table is referenced in Post (Pre) but is missing   --
--  from the Global" - It's not!                                           --
--  This body can be used as the body of a generic declaration or the body --
--  of a general package.  Ths package cannot be generic if it is to       --
--  analysed by SPARK 2014 as it requires the body to be present.          --

pragma SPARK_Mode (Off);
package body SPARK_2014.Dynamic_Tables with
   Refined_State => (The_Table => T)
is
   package Gnat_Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Table_Component_Type,
      Table_Index_Type     => Table_Index_Type,
      Table_Low_Bound      => Table_Low_Bound,
      Table_Initial        => Table_Initial,
      Table_Increment      => Table_Increment);

      T : Gnat_Table.Instance;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index return Table_Last_Type is (Gnat_Table.Last (T))
   with Refined_Global => T;

   -----------------
   -- First_Index --
   -----------------

   function First_Index return Table_Last_Type is (Gnat_Table.First);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean is (Last_Index > First_Index) with
     Refined_Global => The_Table;

   ----------
   -- Init --
   ----------

   procedure Init with
     Refined_Global => (Output => T),
     Refined_Post => Is_Empty
   is
   begin
      Gnat_Table.Init (T);
   end Init;

   ------------
   -- Append --
   ------------

   procedure Append (New_Val : Table_Component_Type) with
     Refined_Global => (In_Out => T)
   is
   begin
      Gnat_Table.Append (T, New_Val);
   end Append;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item (Index : Valid_Table_Index_Type;
                       Item :  Table_Component_Type) with
     Refined_Global => (In_Out => T)
   is
   begin
      T.Table (Index) := Item;
   end Set_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (Index : Valid_Table_Index_Type)
                      return Table_Component_Type is (T.Table (Index))
     with Refined_Global => T;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Val : Table_Last_Type) with
     Refined_Global => (In_Out => T)
   is
   begin
      Gnat_Table.Set_Last (T, New_Val);
   end Set_Last;

end SPARK_2014.Dynamic_Tables;
