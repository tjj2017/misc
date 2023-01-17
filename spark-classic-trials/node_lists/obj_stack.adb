package body Obj_Stack is

   -----------
   -- Count --
   -----------

   function Count (S : Stack_Type) return Nearly_Natural is
   begin
      return S.Count;
   end Count;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Stack_Type) return Boolean is
   begin
      return S.Count = 0;
   end Is_Empty;

   ----------
   -- Init --
   ----------

   procedure Init (S : out Stack_Type) is
   begin
      S. Count := 0;
      --# accept Flow, 31, S.Contents, "Only Count needs initialisation" &
      --#        Flow, 32, S.Contents, "Only Count needs initialisation" &
      --#        Flow, 602, S, S.Contents, "Only Count needs initialisation";
   end Init;

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Stack_Type; V : General_Object.O) is
   begin
      S.Count := S.Count + 1;
      S.Contents (S.Count) := V;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Stack_Type; V : out General_Object.O) is
   begin
      V := S.Contents (S.Count);
      S.Count := S.Count - 1;
   end Pop;

   ---------
   -- Top --
   ---------

   function Top (S : Stack_Type) return General_Object.O is
   begin
      return S.Contents (S.Count);
   end Top;

   -----------------
   -- Predecessor --
   -----------------

   function Predecessor
     (S : Stack_Type; Pred_Num : Nearly_Natural)
      return General_Object.O
   is
   begin
      return S.Contents (S.Count - Pred_Num);
   end Predecessor;

end Obj_Stack;
