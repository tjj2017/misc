package body Bounded_Stacks is

   -----------
   -- Count --
   -----------

   function Count (S : Stack) return Stack_Count
   --# return S.Count;
   is
   begin
      return S.Count;
   end Count;
   pragma Inline (Count);

   ---------------
   -- Not_Empty --
   ---------------

   function Not_Empty (S : Stack) return Boolean
   is
   begin
      return Count (S) > Stack_Count'First;
   end Not_Empty;

  ---------
   -- Top --
   ---------

   function Top (S : Stack) return Element_Type
   --# pre Count (S) > Stack_Count'First;
   --# return S.Contents (Count (S));
   is
   begin
      return S.Contents (Count (S));
   end Top;
   pragma Inline (Top);

   procedure New_Stack (S : out Stack)
  is
   --# hide New_Stack;
   begin
      --  Setting Count to Zero initialises the stack S
      S.Count := 0;
   end New_Stack;
   pragma Inline (New_Stack);

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Stack; Value : Element_Type)
   --# pre S.Count < Stack_Count'Last and Count (S) = S.Count;
   --# post (Top (S) = S.Contents (Count (S))) and
   --#      (S.Contents (Count (S)) = Value) and
   --#      ((S.Count = S~.Count + 1) and (Count (S) = Count (S~) + 1));
   is
   begin
      S.Count := S.Count + 1;
      S.Contents (Count (S)) := Value;
   end Push;
   pragma Inline (Push);

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Stack; Value : out Element_Type)
   --# pre S.Count > Stack_Count'First and Count (S) = S.Count and
   --#       (Not_Empty (S) -> (S.Count > Stack_Count'First));
   --# post S.Count = S~.Count - 1 and Count (S) = S.Count and
   --#      Value = Top (S~);
   is
   begin
      Value := Top (S);
      S.Count := S.Count - 1;
     end Pop;
   pragma Inline (Pop);

   -----------------
   -- Predecessor --
   -----------------

   function Predecessor (S : Stack; Pred_Num : Stack_Count) return Element_Type
   is
   begin
      return S.Contents (S.Count - Pred_Num);
   end Predecessor;
   pragma Inline (Predecessor);

   -----------
   -- Clear --
   -----------

   procedure Clear (S : out Stack)
   is
   --# hide Clear;
   begin
      --  Setting Count to zero means no contents.
      S.Count := 0;
   end Clear;
   pragma Inline (Clear);

end Bounded_Stacks;
