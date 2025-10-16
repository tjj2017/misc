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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Stack) return Boolean
   --# return E => (E <-> S.Count = 0);
   is
   begin
      return S.Count = 0;
   end Is_Empty;

  ---------
   -- Top --
   ---------

   function Top (S : Stack) return Element_Type
   --# pre (Is_Empty (S) <-> S.Count = 0) and S.Count > 0;
   --# return T => T = S.Contents (S.Count);
   is
   begin
      return S.Contents (S.Count);
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
   --# pre Count (S) = S.Count and S.Count < Stack_Count'Last;
   --# post S.Count > 0 and not Is_Empty (S);
   is
   begin
      --# check Is_Empty (S) <-> S.Count = 0;
      S.Count := S.Count + 1;
      --# check Is_Empty (S) <-> S.Count = 0;
      --# check (not Is_Empty (S)) <-> S.Count > 0;
      --# check S.Count > 0;
      --# check not Is_Empty (S);
     S.Contents (S.Count) := Value;
   end Push;
   pragma Inline (Push);

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Stack; Value : out Element_Type)
   --# pre (Is_Empty (S) <-> S.Count = 0) and S.Count > 0 and
   --#     Count (S) = S.Count;
   --# post Count (S) = S~.Count - 1 and
   --#      Value =  Top (S~);
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
