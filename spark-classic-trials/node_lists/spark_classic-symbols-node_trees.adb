package body SPARK_Classic.Symbols.Node_Trees
with Refined_State => (Store => (Tree_Store, In_Progress))
--  --# own Store is Tree_Store, In_Progress;
is
   In_Progress : Boolean;
   Tree_Store : Atrees.Trees.Tree_Type;

   ----------------------
   -- Initialize_Store --
   ----------------------

   procedure Initialize_Store
   is
   begin
      Tree_Store.New_Tree;
      In_Progress := False;
   end Initialize_Store;

   -------------------
   -- Building_List --
   -------------------

   function Building_List
      return Boolean
   is
   begin
      return In_Progress;
   end Building_List;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List (List : Node_List) return Boolean is
   begin
      return List.Tree.Empty_Tree (Tree_Store);
   end Empty_List;

   --------------
   -- New_List --
   --------------

   procedure New_List
     (List : out Node_List)
   is
   begin
      List.Tree.New_Tree (Tree_Store);
      List.Length := 0;
   end New_List;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (N : Types.Node_Id;
      List : in out Node_List;
      Inserted : out Boolean)
   is
   begin
      List.Tree.Insert
        (Key        => N,
         Tree_Store => Tree_Store,
         Inserted   => Inserted);
      if Inserted then
         List.Length := List.Length + 1;
      end if;
   end Insert;

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List
     (List : in out Node_List)
   is
   begin
      In_Progress := False;
   end Save_List;

   ---------------
   -- Are_Equal --
   ---------------

   function Are_Equal
     (List_1 : Node_List;
      List_2 : Node_List)
      return Boolean
   is
   begin
      return List_1.Tree.Is_Equal
        (Tree_2       => List_2.Tree,
         Tree_Store_1 => Tree_Store,
         Tree_Store_2 => Tree_Store);
   end Are_Equal;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present
     (N : Types.Node_Id;
      List : Node_List)
      return Boolean
   is
   begin
      return List.Tree.Is_Present (N, Tree_Store);
   end Is_Present;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth
     (N_List : Node_List)
      return Natural
   is
   begin
      return N_List.Tree.Tree_Depth (Tree_Store);
   end Tree_Depth;

   function Count
     (N_List : Node_list)
      return Natural
   is
   begin
     return N_List.Tree.Count;
   end Count;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator
     (N_List : Node_List)
      return Enumerator
   is
   begin
      return Enumerator'
        (E => Atrees.New_Enumerator
           (Tree       => N_List.Tree,
            Tree_Store => Tree_Store));
   end New_Enumerator;

   ----------
   -- Next --
   ----------

   procedure Next
     (E : in out Enumerator;
      Next_Value: out Types.Node_Id)
   is
      Next_Node : Atrees.Trees.Tree_Node;
   begin
      Atrees.Next_Node
        (E          => E.E,
         Tree_Store => Tree_Store,
         Node       => Next_Node);
      if Tree_Store.Present (Next_Node) then
         Next_Value := Tree_Store.Key (Next_Node);
      else
         Next_Value := Types.Empty;
      end if;
   end Next;

end SPARK_Classic.Symbols.Node_Trees;
