package body SPARK_2005.Multi_ATree.Stack_Ops is

   -----------
   -- Count --
   -----------

   function Count (S : Multi_ATree.Stack) return Multi_Atree.Stack_Count
   --  --# return S.Count;  -- Could be used as a rewrite rule rather than
                            -- user defined simplifier rule if was allowed.
    is
   begin
      return S.Count;
   end Count;
   pragma Inline (Count);

   ------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Multi_ATree.Stack) return Boolean
   --  --# return S.Count = 0;  -- Could be used as a rewrite rule rather than
                                -- user defined simplifier rule if was allowed.
    is
   begin
      return S.Count = 0;
   end Is_Empty;

   procedure New_Stack (S : out Multi_ATree.Stack)
   is
   begin
      --# accept Flow, 32, S.Contents, "Setting Count to zero means no contents" &
      --#        Flow, 31, S.Contents, "As above" &
      --#        Flow, 602, S, S.Contents, "As above";
      S.Count := 0;
   end New_Stack;
   pragma Inline (New_Stack);

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Multi_ATree.Stack;
                   Value : Multi_ATree.Valid_Tree_Node)
   is
   begin
      S.Count := S.Count + 1;
      S.Contents (S.Count) := Value;
      --# check Count (S) = S.Count;
   end Push;
   pragma Inline (Push);

   ---------
   -- Pop --
   ---------

   procedure Pop
     (S : in out Multi_ATree.Stack;
      Value : out Multi_ATree.Valid_Tree_Node)
   is
   begin
      --# check S.Count /= 0;
      Value := S.Contents (S.Count);
      S.Count := S.Count - 1;
      --# check Count (S) = S.Count;
   end Pop;
   pragma Inline (Pop);

   ---------
   -- Top --
   ---------

   function Top
     (S : Multi_ATree.Stack)
      return Multi_ATree.Valid_Tree_Node
   --  --# return S.Contents (S.Count);  --  If this was allowed would provide
   --                                    --  rewrite rule rather than user rule
   is
   begin
      --# check S.Count /= 0;
      return S.Contents (S.Count);
   end Top;
   pragma Inline (Top);

   -----------------
   -- Predecessor --
   -----------------

   function Predecessor
     (S : Multi_ATree.Stack;
      Pred_Num : Multi_Atree.Stack_Index)
      return Multi_Atree.Valid_Tree_Node
   is
   begin
      --# check (S.Count - Pred_Num > 0);
      return S.Contents (S.Count - Pred_Num);
   end Predecessor;
   pragma Inline (Predecessor);

   -----------
   -- Clear --
   -----------

   procedure Clear
     (S : out Multi_ATree.Stack)
   is
   begin
      S.Count := 0;
      --# accept Flow, 31, S.Contents, "Setting Count to zero means no contents" &
      --#        Flow, 32, S.Contents, "As above" &
      --#        Flow, 602, S, S.Contents, "As above";
   end Clear;
   pragma Inline (Clear);

end SPARK_2005.Multi_ATree.Stack_Ops;
