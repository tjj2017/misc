package body SPARK_Classic.Bounded_Stacks is
   --# accept W, 3, "pragma Inline has no semantic effect";

   -----------
   -- Count --
   -----------

   function Count (S : Stack) return Stack_Count
   --# return C => (C = S.Count) and (C = 0 <-> S.Count = 0);
   is
   begin
   	  --# assume S.Count < Stack_Count'Last;
      return S.Count;
   end Count;
   pragma Inline (Count);

    --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (S : Stack)
      return Boolean
   is
   	   Result : Boolean;
   begin
   	  Result := S.Count = 0;
      return Result;
   end Is_Empty;
   pragma Inline (Is_Empty);

   ---------------
   -- New_Stack --
   ---------------

   procedure New_Stack
     (S : out Stack)
   is
   begin
      S.Count := 0;
      --# accept Flow, 32, S.Contents, "Setting Count to zero means no contents" &
      --#        Flow, 31, S.Contents, "As above" &
      --#        Flow, 602, S, S.Contents, "As above";
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
   is
   begin
      return S.Contents (S.Count);
   end Top;
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
   is
   	   --# return S.Count = 0;
   begin
      S.Count := 0;
      --# accept Flow, 32, S.Contents, "Setting Count to zero means no contents" &
      --#        Flow, 31, S.Contents, "As above" &
      --#        Flow, 602, S, S.Contents, "As above";
    end Clear;
   pragma Inline (Clear);

end SPARK_Classic.Bounded_Stacks;
