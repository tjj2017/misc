with Ada.Text_IO; use Ada.Text_IO;
package body SPARK_Classic.Symbols.Node_Trees
with Refined_State => (Store => (Tree_Store, Cache_Store, Cache, In_Progress))
--  --# own Store is Tree_Store, Cache_Store, Cache, In_Progress;
is
   type Cache_Entry is
      record
         List : Node_List;
      end record;

   Null_Cache_Entry : constant Cache_Entry := Cache_Entry'
     (List => Node_List'(Atrees.Null_A_Tree, 0));

   type Cache_Key is mod 2**32;
   Fibonacci_Hash_32 : constant Cache_Key := 2654435769;

   package Node_List_Cache is new SPARK_Classic.Atrees
     (Key_Type   => Cache_Key,
      Value_Type => Cache_Entry,
      Null_Value => Null_Cache_Entry,
      Stack_Size => 32);

   In_Progress : Boolean;
   Tree_Store : Atrees.Trees.Tree_Type;

   Cache_Store : Node_List_Cache.Tree_Type;
   Cache : Node_List_Cache.A_Tree;

   ----------------------
   -- Initialize_Store --
   ----------------------

   procedure Initialize_Store
   is
   begin
      Tree_Store.New_Tree;
      Cache_Store.New_Tree;
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
      return List.Tree.Empty_Tree;
   end Empty_List;

   --------------
   -- New_List --
   --------------

   procedure New_List
     (List : out Node_List)
   is
   begin
      List.Tree.New_Tree;
      List.Length := 0;
      In_Progress := True;
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

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List
     (List : in out Node_List)
     --  --# global in Tree_Store;
     --  --#        in out In_Progress, Cache;
     --  --# pre In_Progress;
     --  --# post not In_Progress;
   is
      Value : constant Cache_Entry := Cache_Entry'(List => List);
      Enum      : Enumerator := Enumerator'
        (E => Atrees.New_Enumerator (List.Tree, Tree_Store));
      Key       : Cache_Key := 0;
      Curr_Node : Types.Node_Id;
      Inserted : Boolean;
      Cached   : Boolean;
      Value_At_Node : Cache_Entry;
   begin
      loop
         Next (Enum, Curr_Node);
         exit when Curr_Node = Types.Empty;
         Key := Key + Cache_Key (Curr_Node);
      end loop;

      Put_Line ("Cache_Key = " & Cache_Key'Image (Key));

      Cached := False;
      while not Cached loop
         Cache.Insert_With_Value
           (Key           => Key,
            Value         => Value,
            Tree_Store    => Cache_Store,
            Inserted      => Inserted,
            Value_At_Node => Value_At_Node);
         if Inserted then
            Cached := True;
            Put_Line ("Inserted");
         --  If not Inserted a Node_List with the same Key exists in the Cache.
         --  Check whether it is the same list of nodes.
         elsif List.Are_Equal (Value_At_Node.List) then
            --  Use the cached list and delete the list being processed.
            List.Tree.Clear (Tree_Store);
            List := Value_At_Node.List;
            Cached := True;
            Put_Line ("Using cached");
         else
            --  The lists of nodes are not the same but they have the same key.
            --  Rehash the key of the List being saved.
            Key := Key * Fibonacci_Hash_32;
            Put_Line ("Key clash, new key = " & Cache_Key'Image (Key));
         end if;
      end loop;
      In_Progress := False;
   end Save_List;

end SPARK_Classic.Symbols.Node_Trees;
