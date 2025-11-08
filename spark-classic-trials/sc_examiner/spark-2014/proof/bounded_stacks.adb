package body Bounded_Stacks
with SPARK_Mode
is
   ---------------
   -- Stack_Abs --
   ---------------

   function Stack_Abs (S : Stack) return Stack_Abstraction is
     ((Contents => Contents_Abstraction (S.Contents), Stack_Top => S.Count));

   -----------
   -- Count --
   -----------

   function Count (S : Stack) return Stack_Count is (S.Count);
   pragma Inline (Count);

   procedure New_Stack
     (S : out Stack)
     with SPARK_Mode => Off
  is
   begin
      --  Setting Count to Zero initialises the stack S
      S.Count := 0;
   end New_Stack;
   pragma Inline (New_Stack);

   ----------
   -- Push --
   ----------

   procedure Push
     (S : in out Stack;
      Value : Element_Type)
   is
   begin
      S.Count := S.Count + 1;
      S.Contents (S.Count) := Value;
   end Push;
   pragma Inline (Push);

   ---------
   -- Pop --
   ---------

   procedure Pop
     (S : in out Stack;
      Value : out Element_Type)
   is
   begin
      Value := S.Contents (S.Count);
      S.Count := S.Count - 1;
   end Pop;
   pragma Inline (Pop);

   ---------
   -- Top --
   ---------

   function Top
     (S : Stack)
      return Element_Type
   is (S.Contents (S.Count));
   --  begin
   --     return S.Contents (S.Count);
   --  end Top;
   pragma Inline (Top);

   -----------------
   -- Predecessor --
   -----------------

   function Predecessor
     (S : Stack;
      Pred_Num : Stack_Count)
      return Element_Type
   is
   begin
      return S.Contents (S.Count - Pred_Num);
   end Predecessor;
   pragma Inline (Predecessor);

   -----------
   -- Clear --
   -----------

   procedure Clear
     (S : out Stack)
     with SPARK_Mode => Off
   is
   begin
      --  Setting Count to zero means no contents.
      S.Count := 0;
   end Clear;
   pragma Inline (Clear);

end Bounded_Stacks;
