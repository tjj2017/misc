package body SPARK_Classic.Symbols.Node_Lists
  with Refined_State => (In_Progress => In_Progress_Flag)
is
   --  In_Progress : Boolean := False;
   In_Progress_Flag : Boolean := False;


   -------------------
   -- Building_List --
   -------------------

   function Building_List return Boolean
     with Refined_Global => In_Progress_Flag
   is
   begin
      return In_Progress_Flag;
   end Building_List;
   pragma Inline (Building_List);

   --------------
   -- New_List --
   --------------

   procedure New_List (Root : out Node_List)
   is
   begin
      In_Progress_Flag := True;
      SPARK_Classic.Symbols.List_Store.Append (Null_Node);
      Root := Node_List (SPARK_Classic.Symbols.List_Store.Last);
   end New_List;

   ------------
   -- Insert --
   ------------

   procedure Insert (N : Types.Node_Id; Root : Node_List)
   is
   begin
      if SPARK_Classic.Symbols.List_Store.Get_Item (Root).Value = Types.Empty
      then
         --  First node of list - make this the root node
   end Insert;

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List (Root : in out Node_List)
   is
   begin
      In_Progress_Flag := False;
   end Save_List;

   -----------------
   --  Is_Present --
   -----------------

   function Is_Present (N : Types.Node_Id; Root : Node_List) return Boolean
   is
   begin
      return False;
   end Is_Present;

end SPARK_Classic.Symbols.Node_Lists;
