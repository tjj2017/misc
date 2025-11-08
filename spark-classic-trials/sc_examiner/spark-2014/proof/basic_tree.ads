---------------------------------- Basic_Tree --------------------------------
--  This package is a stateless abstraction providing basic operations on   --
--  an underlying tree. The body of this package must be excluded from SPARK--
--  analysis because of the hidden state.                                   --
--                                                                          --
--  The underlying data type use for tree storage and management is the     --
--  limited type Tree.  Individual nodes of the tree are accessed via the   --
--  private type Node_Index.  A Node_Index object can only access a node    --
--  from a Tree object which generated the Node_Index object.               --
--  A Node_Index contains a numeric value and as nodes are added to the     --                                                                        --
--  using Add_Node which returns a Node_Index to access the new node.       --
--  Each node added has a Node_Index which has contains a numeric value     --
--  which monotonically increases with each node added.                     --
--                                                                          --
--  The type, Node_Index_Range, defines the range of values a Node_Index    --
--  may have and must have the                               --
--  range 0 .. Maximum_Number_Of_Nodes in the tree.                         --
--  Node_Index_Range'First represents an Index to No_Node.
--
--  Each node of the Tree has 5 fields:                                     --
--  Left, Right : Tree_Node;   --  the children of the node                 --
--  Level       : Level_Type;  --  the level of the node in the Tree        --
--  Key         : Key_Type;    --  place for key of node                    --
--  Value       : Value_Type;  --  place for value of node                  --
--  Both Key and Value need to have a null value provided.                  --
--  The values of these fields are set and interrogated by the subprograms  --
--  declared below.                                                         --
--  Procedure Init should be called prior to using the tree structure.      --
------------------------------------------------------------------------------

with GNAT.Dynamic_Tables;
generic
   --  Node_Index must have a range as the number of possible nodes in the tree.
   --  Each Node_Index references a particular node in the tree.
   type Node_Index is range <>;

   --  Level is a component of each node in the tree and is used to indicate
   --  the level at which the node is positioned in the tree it have a range
   --  which covers all possible levels of the tree.
   type Level_Type is range <>;

   --  Each node has a key.
   --  The key type must satisfy an order relation.
   type Key_Type is (<>);

   --  The value component of each node.
   type Value_Type is private;

   Null_Key : Key_Type;
   Null_Value : Value_Type;

package Basic_Tree is

   type Tree is limited private;  -- Limited type because it uses pointers.

   --  Each node added to a tree has a Node_Index the value of which
   --  increases consecutuvely as nodes are added.  If two trees have the same
   --  number of nodes, each will have the same values of Node_Index to
   --  reference the nodes.
   --
   --  Node_Index'First represents a null index that does not reference any
   --  node in the tree.
   Null_Index : constant Node_Index := Node_Index'First;

   --  A valid Node Index excludes the Null_Index.
   First_Valid_Node_Index : constant Node_Index :=
     Node_Index'Succ (Node_Index'First);
   subtype Valid_Node_Index is Node_Index range
     First_Valid_Node_Index .. Node_Index'Last;

   -- The first Node_Index is a constant value of the First_Valid_Node
   -- even if the tree is empty and cannot be used to index a node unless
   -- the tree is non-empty.
   First_Node_Index : constant Valid_Node_Index := First_Valid_Node_Index;

   --  Proof function to indicate the Tree is empty (has no nodes) after an
   --  initialization (by calling Init.
   --  A tree can only become not empty by adding a node (by calling Add_Node).
   function Empty_Tree (T : Tree) return Boolean with
     Ghost;

   --  Proof function to representing the last Node_Index in the Tree, T.
   --  It's value is Null_Index if the tree is empty.
   function Last_Index (T : Tree) return Node_Index with
     Ghost;

   --  Proof function stating that the given Node_Index does, indeed,
   --  reference a node in the tree.
   --  A Node Index onl references a node in the tree if it is the Node_Index
   --  returned fom a call to Add_Node.
   function In_Tree (T : Tree; I : Node_Index) return Boolean with
     Pre =>  not Empty_Tree (T),
     Post => In_Tree'Result = (I in Valid_Node_Index and I <= Last_Index (T)),
     Ghost;

   -- This procedure must be called prior to using an object of type Tree
   -- but should not be used thereafter.
   procedure Init (T : out Tree) with
     Post => Empty_Tree(T) and Last_Index (T)  = Null_Index;

   -- This procedure resets the Tree, T, to empty and reinitializes T.
   -- Do not recall Init.
   procedure Reset (T : in out Tree) with
     Post => Empty_Tree(T) and Last_Index (T)  = Null_Index;

   function Last_Node_Index (T : Tree) return Node_Index with
     Pre  => not Empty_Tree (T),
     Post => Last_Node_Index'Result = Last_Index (T) and
             In_Tree (T, Last_Node_Index'Result),
     Inline;

   function Level (T : Tree; I : Valid_Node_Index) return Level_Type with
     Pre  => not Empty_Tree (T) and In_Tree (T, I),
     Inline;

   -- Left returns Null_Index if the node referenced by the Node_Index has
   -- no left child otherwise it returns the Node_Index of the left child.
   function Left  (T : Tree; I : Valid_Node_Index) return Node_Index with
     Pre  => not Empty_Tree (T) and In_Tree (T, I),
     Post => (if Left'Result /= Null_Index then In_Tree (T, Left'Result)),
     Inline;

   -- Right returns Null_Index if the node referenced by the Node_Index has
   -- no right child otherwise it returns the Node_Index of the right child.
   function Right (T : Tree; I : Valid_Node_Index) return Node_Index with
     Pre  => not Empty_Tree (T) and In_Tree (T, I),
     Post => (if Right'Result /= Null_Index then In_Tree (T, Right'Result)),
     Inline;

   function Key (T : Tree; I : Valid_Node_Index) return Key_Type with
     Pre  => not Empty_Tree (T) and In_Tree (T, I),
     Inline;

   function Value (T : Tree; I : Valid_Node_Index) return Value_Type with
     Pre  => not Empty_Tree (T) and In_Tree (T, I),
     Inline;

   -----------------------------------------------------------------------------
   -- The following proof functions are used to specify the effects of the    --
   -- interface procedures on the given Tree (an in out parameter).           --
   -----------------------------------------------------------------------------

   function Node_Contents_Equivalence
     (T1, T2 : Tree; I1, I2 : Valid_Node_Index) return Boolean is
     (Key (T1, I1) = Key (T2, I2) and Value (T1, I1) = Value (T2, I2)) with
     Pre  =>  not Empty_Tree (T1) and not Empty_Tree (T2) and
            In_Tree (T1, I1) and In_Tree (T2, I2),
     Ghost;

   function Node_Structure_Equivalence
     (T1, T2 : Tree; I1, I2 : Node_Index) return Boolean is
     (Level (T1, I1) = Level (T2, I2) and
          Left (T1, I1) = Left (T2, I2) and
          Right (T1, I1) = Right (T2, I2)) with
      Pre  => not Empty_Tree (T1) and not Empty_Tree (T2) and
              In_Tree (T1, I1) and In_Tree (T2, I2),
      Ghost;

   --  The following proot functions are used to indicate the persistence of a
   --  tree after calling a procedure that modifies an aspect of a tree.
   --  the order in which the nodes are added are expected to be identical
   --  for each tree and therefore the Node_Index of each node of one tree
   --  corresponds exactly to the Node_Index of the second_tree.
   --  Hence, only singe start Node_Index, S, and a single end Node_Index, E,
   --  need be given to specify the range of nodes to which the functions
   --  apply.
   function Contents_Equal (T1, T2 : Tree; S, E : Valid_Node_Index)
      return Boolean is
      (In_Tree (T1, S) and In_Tree (T2, S) and
           In_Tree (T2, E) and In_Tree (T2, E)) and then
          (for all I in Node_Index range S .. E =>
               (Node_Contents_Equivalence (T1, T2, I, I))) with
      Pre  => not Empty_Tree (T1) and not Empty_Tree (T2),
      Ghost;

   function Structure_Equal (T1, T2 : Tree; S, E : Valid_Node_Index)
      return Boolean is
        ((In_Tree (T1, S) and In_Tree (T2, S) and
              In_Tree (T2, E) and In_Tree (T2, E)) and then
             (for all I in Node_Index range S .. E =>
                  (Node_Structure_Equivalence (T1, T2, I, I)))) with
      Pre  => not Empty_Tree (T1) and not Empty_Tree (T2),
      Ghost;

   function Contents_Preserved_Between
     (T1, T2 : Tree;       --  T1 and T2 have the same Node_Index values
      S, E,                --  between start, S, and end, E, Indices.
      Except : Node_Index) --  Exclude this node.
                           --  Null_Index -> no exclusion.
      return Boolean is
     ((In_Tree (T1, S) and In_Tree (T2, S) and
         In_Tree (T1, E) and In_Tree (T2, E) and
         In_Tree (T1, Except) and In_Tree (T2, Except)) and then
        (if Except = Null_Index then
           Contents_Equal (T1, T2, S, E)
         elsif Except = S then
           Contents_Equal (T1, T2, S + 1, E)
         elsif Except = E then
           Contents_Equal (T1, T2, S, E - 1)
        else
           Contents_Equal (T1, T2, S, Except - 1) and
           Contents_Equal (T1, T2, Except + 1, E))) with
   Pre  => not Empty_Tree (T1) and not Empty_Tree (T2) and
           ((Except = Null_Index or else
                 (S <= E and Except >= S and Except <= E))),
   Ghost;

   --# function Contents_Preserved_Except (T1, T2 : Tree;
   --#                                     Except : Node_Index)
   --#                                     return Boolean;
   --# pre not Empty_Tree (T1) and not Empty_Tree (T2)  and
   --#         (Except = Null_Index or else (Except in Valid_Node_Index and
   --#            Except <= Last_Node_Index (T1) and
   --#            Except <= Last_Node_Index (T2)));
   --# return Contents_Preserved_Between (T1, T2,
   --#                                    First_Node_Index,
   --#                                    Last_Node_Index (T1),
   --#                                    Except);

   --# function Contents_Preserved (T1, T2 : Tree) return Boolean;
   --# pre not Empty_Tree (T1) and not Empty_Tree (T2);
   --# return Contents_Preserved_Except (T1, T2, Null_Index);

   --# function Structure_Preserved_Between
   --#    (T1, T2 : Tree;       --  T1 and T2 have the same Node_Index values
   --#     S, E,                --  between start, S, and end, E, Indices.
   --#     Except : Node_Index) --  Exclude this node.
   --#                          --  Null_Index -> no exclusion.
   --#     return Boolean;
   --# pre not Empty_Tree (T1) and not Empty_Tree (T2) and
   --#     ((Except = Null_Index or else
   --#         (S <= E and Except >= S and Except <= E)));
   --# return (In_Tree (T1, S) and In_Tree (T2, S) and
   --#         In_Tree (T1, E) and In_Tree (T2, E) and
   --#         In_Tree (T1, Except) and In_Tree (T2, Except)) and then
   --#            (((Except = Null_Index) ->
   --#                Structure_Equal (T1, T2, S, E))
   --#             and
   --#             ((Except = S) ->
   --#                Structure_Equal (T1, T2, S + 1, E))
   --#             and
   --#             ((Except = E) ->
   --#                Structure_Equal (T1, T2, S, E - 1))
   --#             and
   --#             ((Except /= Null_Index and Except /= S and Except /= E) ->
   --#                (Structure_Equal (T1, T2, S, Except - 1)
   --#                 and
   --#                 Structure_Equal (T1, T2, Except + 1, E))));

   --# function Structure_Preserved_Except (T1, T2 : Tree;
   --#                                      Except : Node_Index) return Boolean;
   --# pre not Empty_Tree (T1) and not Empty_Tree (T2) and
   --#         (Except = Null_Index or else (Except in Valid_Node_Index and
   --#            Except <= Last_Node_Index (T1) and
   --#            Except <= Last_Node_Index (T2)));
   --# return Structure_Preserved_Between (T1, T2,
   --#                                     First_Node_Index,
   --#                                     Last_Node_Index (T1),
   --#                                     Except);

   --# function Structure_Preserved (T1, T2 : Tree) return Boolean;
   --# pre not Empty_Tree (T1) and not Empty_Tree (T2);
   --# return Structure_Preserved_Except (T1, T2, Null_Index);

   --# function Bounds_Preserved (T1, T2 : Tree) return Boolean;
   --# return Last_Node_Index  (T1) = Last_Node_Index (T2);

   -----------------------------------------------------------------------------
   --  Procedures that update the Tree parameter                              --
   -----------------------------------------------------------------------------

   procedure Set_Level (T : in out Tree; I : Valid_Node_Index;
                        Node_Level : Level_Type);
   --# pre  not Empty_Tree (T) and In_Tree (T, I);
   --# post Bounds_Preserved (T, T~) and
   --#      Contents_Preserved (T, T~) and
   --#      Structure_Preserved_Except (T, T~, I) and
   --#      not Empty_Tree (T) and In_Tree (T, I) and
   --#      Left (T, I)  = Left (T~, I) and
   --#      Right (T, I) = Right (T~, I) and
   --#      Level (T, I) = Node_Level;
   pragma Inline (Set_Level);

   procedure Set_Left  (T : in out Tree;
                        I : Valid_Node_Index;
                        Branch : Valid_Node_Index);
   --# pre  not Empty_Tree (T) and In_Tree (T, I);
   --# post --  Bounds_Preserved (T, T~); -- and
   --#      --  Contents_Preserved (T, T~); --  and
   --#      --  Structure_Preserved_Except (T, T~, I) and
   --#      not Empty_Tree (T) and In_Tree (T, I) and
   --#      Left (T, I)  = Branch; -- and
   --  --#      Right (T, I) = Right (T~, I) and
   --  --#      Level (T, I) = Level (T~, I);
   pragma Inline (Set_Left);

   procedure Set_Right (T : in out Tree;
                        I : Valid_Node_Index;
                        Branch : Valid_Node_Index);
   --# pre  not Empty_Tree (T) and In_Tree (T, I);
   --  --# post Bounds_Preserved (T, T~) and
   --  --#      Contents_Preserved (T, T~) and
   --  --#      Structure_Preserved_Except (T, T~, I) and
   --  --#      not Empty_Tree (T) and In_Tree (T, I) and
   --  --#      Left (T, I)  = Left (T~, I) and
   --  --#      Right (T, I) = Branch and
   --  --#      Level (T, I) = Level (T~, I);
   pragma Inline (Set_Right);

   procedure Set_Key (T : in out Tree;
                      I : Valid_Node_Index;
                      The_Key : Key_Type);
   --# pre  not Empty_Tree (T) and In_Tree (T, I);
   --# post Bounds_Preserved (T, T~) and
   --#      Structure_Preserved (T, T~) and
   --#      Contents_Preserved_Except (T, T~, I) and
   --#      not Empty_Tree (T) and In_Tree (T, I) and
   --#      Key (T, I) = The_Key and
   --#      Value (T, I) = Value (T~, I);
   pragma Inline (Set_Key);

   procedure Set_Value (T : in out Tree;
                        I : Valid_Node_Index;
                        Node_Value : Value_Type);
   --# pre  not Empty_Tree (T) and In_Tree (T, I);
   --# post Bounds_Preserved (T, T~) and
   --#      Structure_Preserved (T, T~) and
   --#      Contents_Preserved_Except (T, T~, I) and
   --#      not Empty_Tree (T) and In_Tree (T, I) and
   --#      Key (T, I) = Key (T~, I) and
   --#      Value (T, I) = Node_Value;
   pragma Inline (Set_Value);

   procedure Add_Node  (T         : in out Tree;
                        New_Index : out Valid_Node_Index;
                        The_Key   : Key_Type);
   --# post (not Empty_Tree (T~) ->
   --#          (Structure_Preserved_Except (T, T~, Last_Node_Index (T)) and
   --#           Contents_Preserved_Except (T, T~, Last_Node_Index (T~)))) and
   --#      Last_Index (T) = Last_Node_Index (T) and
   --#      Last_Node_Index (T) = Last_Node_Index (T~) + 1 and
   --#      Last_Node_Index (T) >= First_Node_Index and
   --#      not Empty_Tree (T) and In_Tree (T, New_Index) and
   --#      Key (T, Last_Node_Index (T)) = The_Key and
   --#      Value (T, Last_Node_Index (T)) = Null_Value and
   --#      Level (T, Last_Node_Index (T)) = Level_Type'First and
   --#      Left (T, Last_Node_Index (T)) = Null_Index and
   --#      Right (T, Last_Node_Index (T)) = Null_Index and
   --#      New_Index = Last_Node_Index (T);
   pragma Inline (Add_Node);

   procedure Clear_Tree_Below_Node (T : in out Tree;
                                    Final_Node : Valid_Node_Index);
   --# pre not Empty_Tree (T) and In_Tree (T, Final_Node);
   --# post not Empty_Tree (T) and In_Tree (T, Final_Node) and
   --#      Last_Node_Index (T) = Final_Node and
   --#      Structure_Preserved_Between (T, T~,
   --#                 First_Node_Index, Final_Node, Null_Index) and
   --#      Contents_Preserved_Between (T, T~,
   --#                 First_Node_Index, Final_Node, Null_Index);
   pragma Inline (Clear_Tree_Below_Node);
   --  Removes all nodes from the Tree with Tree_Node values > IV (Final_Node).

private
   --# hide Basic_Tree;  -- Tree uses pointer type.
                         -- Protected by making Tree a limited type.

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Level_Type;
         Left  : Node_Index;
         Right : Node_Index;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => Level_Type'First,
      Left  => Null_Index,
      Right => Null_Index);

   package Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Node_Index,
      Table_Low_Bound      => First_Valid_Node_Index,
      Table_Initial        => 100,
      Table_Increment      => 100);

   type Tree is New Table.Instance;

end Basic_Tree;
