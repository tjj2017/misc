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

with Specific_Tree_Types;
with GNAT.Dynamic_Tables;
--# inherit Specific_Tree_Types;
package Basic_Tree is

   type Tree is limited private;
   type Node_Index is private;

   subtype Node_Index_Range is Specific_Tree_Types.Tree_Node;
   subtype Level_Type is Specific_Tree_Types.Level_Type;
   subtype Key_Type is Specific_Tree_Types.Key_Type;
   subtype Value_Type is Specific_Tree_Types.Value_Type;
   Null_Key : constant Key_Type := Specific_Tree_Types.Null_Key;
   Null_Value : constant Value_Type := Specific_Tree_Types.Null_Value;
   No_Node    : constant Node_Index;

   function Not_Empty_Tree (T : Tree) return Boolean;
   pragma Inline (Not_Empty_Tree);

   procedure Init (T : out Tree);
   --# post not Not_Empty_Tree(T);

   function IV (T : Tree; I : Node_Index) return Node_Index_Range;
   --# pre Not_Empty_Tree (T);
   --  Each Node_Index has an associated numerical value.
   --  IV returns the numerical value part of a Node Index.

   function First_Node_Index (T : Tree) return Node_Index;
   pragma Inline (First_Node_Index);
   --  Returns Lowest Node_Index of the Tree.

   function Last_Node_Index (T : Tree) return Node_Index;
   pragma Inline (Last_Node_Index);
   --  Returns Index to the last node added to the tree - No_Node
   --  if tree is empty.

   function In_Tree  (T : Tree; I : Node_Index) return Boolean;
   --# return Not_Empty_Tree (T) and
   --#        IV (T, I) >= IV (T, First_Node_Index (T)) and
   --#        IV (T, I) <= IV (T, Last_Node_Index (T));
   pragma Inline (In_Tree);

   function Tree_Leaf (T : Tree; I : Node_Index) return Boolean;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --  True if the Node_Index references a leaf of the Tree.
   pragma Inline (Tree_Leaf);

   function Level (T : Tree; I : Node_Index) return Level_Type;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   pragma Inline (Level);

   function Left  (T : Tree; I : Node_Index) return Node_Index;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# return C => In_Tree (T, C);
   pragma Inline (Left);

   function Right (T : Tree; I : Node_Index) return Node_Index;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# return C => In_Tree (T, C);
   pragma Inline (Right);

   function Key (T : Tree; I : Node_Index) return Key_Type;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   pragma Inline (Key);

   function Value (T : Tree; I : Node_Index) return Value_Type;
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   pragma Inline (Value);

   -----------------------------------------------------------------------------
   -- The following proof functions are used to specify the effects of the    --
   -- interface procedures on the given Tree (an in out parameter).           --
   -----------------------------------------------------------------------------

   --# function To_Node_Index
   --#    (T : Tree; NR : Node_Index_Range) return Node_Index;
   --# pre Not_Empty_Tree (T);
   --# return I => (I /= No_Node) -> In_Tree (T, I);
   --  Given A Tree and a Node_Index_Range value, returns
   --  a Node_Index if the Node_Index_Range value is a valid
   --  index into the Tree.  Otherwise returns No_Node;

   --# function Node_Contents_Equivalence
   --#    (T1, T2 : Tree; I1, I2 : Node_Index) return Boolean;
   --# return Key (T1, I1) = Key (T2, I2) and Value (T1, I1) = Value (T2, I2);

   --# function Node_Structure_Equivalence
   --#    (T1, T2 : Tree; I1, I2 : Node_Index) return Boolean;
   --# return Level (T1, I1) = Level (T2, I2) and
   --#        Left (T1, I1) = Left (T2, I2) and
   --#        Right (T1, I1) = Right (T2, I2);

   --# function Contents_Equal (T1, T2 : Tree; S, E : Node_Index_Range)
   --#                         return Boolean;
   --# return for all NR in Node_Index_Range range S .. E =>
   --#              (Node_Contents_Equivalence (T1, T2,
   --#                                          To_Node_Index (T1, NR),
   --#                                          To_Node_Index (T2, NR)));

   --# function Structure_Equal (T1, T2 : Tree; S, E : Node_Index_Range)
   --#                         return Boolean;
   --# return for all NR in Node_Index_Range range S .. E =>
   --#              (Node_Structure_Equivalence (T1, T2,
   --#                                           To_Node_Index (T1, NR),
   --#                                           To_Node_Index (T2, NR)));

   --# function Contents_Preserved_Between
   --#    (T1, T2 : Tree;        --  T1 and T2 have the same Node_Index values
   --#     S, E   : Node_Index;  --  between start, S, and end, E, Indices.
   --#     Except : Node_Index)  --  Exclude this node - No_Node = no exclusion.
   --#     return Boolean;
   --# pre Not_Empty_Tree (T1) and Not_Empty_Tree (T2)  and
   --#     In_Tree (T1, S) and In_Tree (T2, S) and
   --#     In_Tree (T1, E) and In_Tree (T2, E) and
   --#     In_Tree (T1, Except) and In_Tree (T2, Except) and
   --#     IV (T1, S) < IV (T1, E) and
   --#     IV (T1, Except) >= IV (T1, S) and IV (T1, Except) <= IV (T1, E) and
   --#     IV (T1, S) = IV (T2, S) and IV (T1, E) = IV (T2, E);
   --# return (((Except = No_Node) ->
   --#             Structure_Equal (T1, T2, IV (T1, S), IV (T1, E)))
   --#         and
   --#         ((Except = S) ->
   --#             Contents_Equal (T1, T2, IV (T1, S) + 1, IV (T1, E)))
   --#         and
   --#         ((Except = E) ->
   --#             Contents_Equal (T1, T2, IV (T1, S), IV (T1, E) - 1))
   --#          and
   --#          ((Except /= No_Node and Except /= S and Except /= E) ->
   --#             (Contents_Equal (T1, T2, IV (T1, S), IV (T1, Except) - 1)
   --#              and
   --#              Contents_Equal (T1, T2, IV (T1, Except) - 1, IV (T1, E)))));

    --# function Contents_Preserved_Except (T1, T2 : Tree;
   --#                                     Except : Node_Index) return Boolean;
   --# pre IV (T1, First_Node_Index (T1)) = IV (T2, First_Node_Index (T2)) and
   --#     IV (T1, Last_Node_Index (T1))  = IV (T2, Last_Node_Index (T2)) and
   --#     (Except = No_Node or else
   --#        (IV (T1, Except) >= IV (T1, First_Node_Index (T1)) and
   --#         IV (T1, Except) <= IV (T1, Last_Node_Index (T1))));
   --# return Contents_Preserved_Between (T1, T2,
   --#                                    First_Node_Index (T1),
   --#                                    Last_Node_Index (T1),
   --#                                    Except);

   --# function Contents_Preserved (T1, T2 : Tree) return Boolean;
   --# pre IV (T1, First_Node_Index (T1)) = IV (T2, First_Node_Index (T2)) and
   --#     IV (T1, Last_Node_Index (T1))  = IV (T2, Last_Node_Index (T2));
   --# return Contents_Preserved_Except (T1, T2, No_Node);

  --# function Structure_Preserved_Between
  --#    (T1, T2 : Tree;        --  T1 and T2 have the same Node_Index values
  --#     S, E   : Node_Index;  --  between start, S, and end, E, Indices.
  --#     Except : Node_Index)  --  Exclude this node - No_Node = no exclusion.
  --#     return Boolean;
  --# pre Not_Empty_Tree (T1) and Not_Empty_Tree (T2)  and
  --#     In_Tree (T1, S) and In_Tree (T2, S) and
  --#     In_Tree (T1, E) and In_Tree (T2, E) and
  --#     In_Tree (T1, Except) and In_Tree (T2, Except) and
  --#     IV (T1, S) < IV (T1, E) and
  --#     IV (T1, Except) >= IV (T1, S) and IV (T1, Except) <= IV (T1, E) and
  --#     IV (T1, S) = IV (T2, S) and IV (T1, E) = IV (T2, E);
  --# return (((Except = No_Node) ->
  --#           Structure_Equal (T1, T2, IV (T1, S), IV (T1, E)))
  --#         and
  --#         ((Except = S) ->
  --#           Structure_Equal (T1, T2, IV (T1, S) + 1, IV (T1, E)))
  --#         and
  --#         ((Except = E) ->
  --#           Structure_Equal (T1, T2, IV (T1, S), IV (T1, E) - 1))
  --#          and
  --#          ((Except /= No_Node and Except /= S and Except /= E) ->
  --#            (Structure_Equal (T1, T2, IV (T1, S), IV (T1, Except) - 1) and
  --#             Structure_Equal (T1, T2, IV (T1, Except) - 1, IV (T1, E)))));

   --# function Structure_Preserved_Except (T1, T2 : Tree;
   --#                                      Except : Node_Index) return Boolean;
   --# pre IV (T1, First_Node_Index (T1)) = IV (T2, First_Node_Index (T2)) and
   --#     IV (T1, Last_Node_Index (T1))  = IV (T2, Last_Node_Index (T2)) and
   --#     (Except = No_Node or else
   --#        (IV (T1, Except) >= IV (T1, First_Node_Index (T1)) and
   --#         IV (T1, Except) <= IV (T1, Last_Node_Index (T1))));
   --# return Structure_Preserved_Between (T1, T2,
   --#                                     First_Node_Index (T1),
   --#                                     Last_Node_Index (T1),
   --#                                     Except);

   --# function Structure_Preserved (T1, T2 : Tree) return Boolean;
   --# pre IV (T1, First_Node_Index (T1)) = IV (T2, First_Node_Index (T2)) and
   --#     IV (T1, Last_Node_Index (T1))  = IV (T2, Last_Node_Index (T2));
   --# return Structure_Preserved_Except (T1, T2, No_Node);

   -----------------------------------------------------------------------------
   --  Procedures that update the Tree parameter                              --
   -----------------------------------------------------------------------------

   procedure Set_Level (T : in out Tree; I : Node_Index;
                        Node_Level : Level_Type);
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# post Contents_Preserved (T, T~) and
   --#      Structure_Preserved_Except (T, T~, I) and
   --#      Not_Empty_Tree (T) and In_Tree (T, I) and
   --#      Left (T, I)  = Left (T~, I) and
   --#      Right (T, I) = Right (T~, I) and
   --#      Level (T, I) = Node_Level;
   pragma Inline (Set_Level);

   procedure Set_Left  (T : in out Tree; I : Node_Index; Branch : Node_Index);
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# post Contents_Preserved (T, T~) and
   --#      Structure_Preserved_Except (T, T~, I) and
   --#      Not_Empty_Tree (T) and In_Tree (T, I) and
   --#      Left (T, I)  = Branch and
   --#      Right (T, I) = Right (T~, I) and
   --#      Level (T, I) = Level (T~, I);
   pragma Inline (Set_Left);

   procedure Set_Right (T : in out Tree; I : Node_Index; Branch : Node_Index);
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# post Contents_Preserved (T, T~) and
   --#      Structure_Preserved_Except (T, T~, I) and
   --#      Not_Empty_Tree (T) and In_Tree (T, I) and
   --#      Left (T, I)  = Left (T~, I) and
   --#      Right (T, I) = Branch and
   --#      Level (T, I) = Level (T~, I);
   pragma Inline (Set_Right);

   procedure Set_Key (T : in out Tree; I : Node_Index; The_Key : Key_Type);
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# post Structure_Preserved (T, T~) and
   --#      Contents_Preserved_Except (T, T~, I) and
   --#      Not_Empty_Tree (T) and In_Tree (T, I) and
   --#      Key (T, I) = The_Key and
   --#      Value (T, I) = Value (T~, I);
   pragma Inline (Set_Key);

   procedure Set_Value (T : in out Tree;
                        I : Node_Index;
                        Node_Value : Value_Type);
   --# pre Not_Empty_Tree (T) and In_Tree (T, I);
   --# post Structure_Preserved (T, T~) and
   --#      Contents_Preserved_Except (T, T~, I) and
   --#      Not_Empty_Tree (T) and In_Tree (T, I) and
   --#      Key (T, I) = Key (T~, I) and
   --#      Value (T, I) = Node_Value;
   pragma Inline (Set_Value);

   procedure Add_Node  (T        : in out Tree;
                        New_Node : out Node_Index;
                        The_Key  : Key_Type);
   --# post IV (T, Last_Node_Index (T)) = IV (T~, Last_Node_Index (T~)) + 1 and
   --#      Not_Empty_Tree (T) and In_Tree (T, New_Node) and
   --#      Structure_Preserved_Between (T, T~,
   --#                 First_Node_Index (T~), Last_Node_Index (T~), No_Node) and
   --#      Contents_Preserved_Between (T, T~,
   --#                 First_Node_Index (T~), Last_Node_Index (T~), No_Node) and
   --#      Key (T, Last_Node_Index (T)) = The_Key and
   --#      Value (T, Last_Node_Index (T)) = Null_Value and
   --#      Level (T, Last_Node_Index (T)) = Level_Type'First and
   --#      Tree_Leaf (T, Left  (T, Last_Node_Index (T))) and
   --#      Tree_Leaf (T, Right (T, Last_Node_Index (T)));
   pragma Inline (Add_Node);

   procedure Clear_Tree_Below_Node (T : in out Tree; Final_Node : Node_Index);
   --# pre Not_Empty_Tree (T) and In_Tree (T, Final_Node);
   --# post Not_Empty_Tree (T) and In_Tree (T, Final_Node) and
   --#      Last_Node_Index (T) = Final_Node and
   --#      Structure_Preserved_Between (T, T~,
   --#                 First_Node_Index (T~), Final_Node, No_Node) and
   --#      Contents_Preserved_Between (T, T~,
   --#                 First_Node_Index (T~), Final_Node, No_Node);
   pragma Inline (Clear_Tree_Below_Node);
   --  Removes all nodes from the Tree with Tree_Node values > IV (Final_Node).

private
   --# hide Basic_Tree;  -- Tree uses pointer type.
                         -- Protected by making Tree a limited type.

   Null_Index : constant Node_Index_Range := Node_Index_Range'First;

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Level_Type;
         Left  : Node_Index_Range;
         Right : Node_Index_Range;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => Level_Type'First,
      Left  => Null_Index,
      Right => Null_Index);

   package Table is new GNAT.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Node_Index_Range,
      Table_Low_Bound      => Node_Index_Range'Succ (Node_Index_Range'First),
      Table_Initial        => 100,
      Table_Increment      => 100);

   type Tree is New Table.Instance;

   type Node_Index is
      record
         Tree_Id : Natural;
         Index   : Node_Index_Range;
      end record;

   No_Node : constant Node_Index := Node_Index'
     (Tree_Id => 0,
      Index   => Node_Index_Range'First);

end Basic_Tree;
