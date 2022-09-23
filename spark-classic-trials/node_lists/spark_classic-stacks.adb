pragma SPARK_Mode;
package body SPARK_Classic.Stacks is

   -----------
   -- Count --
   -----------

   function Count (S : Stack_Type) return Nearly_Natural is
   begin
      return S.Count;
   end Count;
   pragma Inline (Count);

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Stack_Type) return Boolean is
   begin
      return S.Count = 0;
   end Is_Empty;
   pragma Inline (Is_Empty);

   ---------------
   -- New_Stack --
   ---------------

   procedure New_Stack (S : out Stack_Type) is
   begin
      Dynamic_Stack.Init (S.Contents);
      pragma Assert (Dynamic_Stack.First = 1);
      S.Count := 0;
   end New_Stack;

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Stack_Type; Value : Element_Type) is
   begin
      S.Count := S.Count + 1;
      --  Use Set_Item as the stack may have to be extends
      Dynamic_Stack.Set_Item
        (T     => S.Contents,
         Index => S.Count,
         Item  => Value);
   end Push;
   pragma Inline (Push);

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Stack_Type; Value : out Element_Type) is
   begin
      Value := S.Contents.Table (S.Count);
      S.Count := S.Count - 1;
   end Pop;
   pragma Inline (Pop);

   ---------
   -- Top --
   ---------

   function Top (S : Stack_Type) return Element_Type is
   begin
      return S.Contents.Table (S.Count);
   end Top;
   pragma Inline (Top);

   -----------------
   -- Predecessor --
   -----------------

   function Predecessor (S : Stack_Type; Pred_Num : Nearly_Natural)
                         return Element_Type is
   begin
      return S.Contents.Table (S.Count - Pred_Num);
   end Predecessor;
   pragma Inline (Predecessor);

   -----------
   -- Clear --
   -----------

   procedure Clear (S : in out Stack_Type) is
   begin
      S.Count := 0;
   end Clear;

end SPARK_Classic.Stacks;
