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
package Basic_Tree is

   type Tree is limited private;
   type Node_Index is private;
   type Valid_Node_Index is private;

   subtype Node_Index_Range is Specific_Tree_Types.Tree_Node;
   subtype Level_Type is Specific_Tree_Types.Level_Type;
   subtype Key_Type is Specific_Tree_Types.Key_Type;
   subtype Value_Type is Specific_Tree_Types.Value_Type;
   Null_Key : constant Key_Type := Specific_Tree_Types.Null_Key;
   Null_Value : constant Value_Type := Specific_Tree_Types.Null_Value;
   No_Node : constant Node_Index;

   procedure Init (T : out Tree);


   function Is_Empty_Tree (T : Tree) return Boolean;

   function Base_Node_Index (T : Tree) return Node_Index with
     Inline;

   function Last_Node_Index (T : Tree) return Node_Index with
     Inline;
   --  Returns the last node added to the tree - No_Node
   --  if tree is empty.

   function Root (T : Tree) return Node_Index with
     Inline;

   function Is_A_Valid_Node_Index (T : Tree; N : Node_Index) return Boolean with
     Inline;

   function In_Tree  (T : Tree; N : Node_Index) return Boolean with
     Post   => (if In_Tree'Result then
                  not Is_Empty_Tree and Is_A_Valid_Tree_Node (N) and
                  N in Valid_Tree_Node),
     Inline;

   function Level (N : Valid_Tree_Node) return Level_Type with
     --  Global => Tree_Store,
     Pre => not Is_Empty_Tree,
     Inline;

   function Left  (N : Valid_Tree_Node) return Tree_Node with
     --  Global => Tree_Store,
     Pre    => not Is_Empty_Tree,
     Post   => (if Left'Result /= Empty_Node then
                  In_Tree (Left'Result)),
     Inline;
   function Right (N : Valid_Tree_Node) return Tree_Node with
     --  Global => Tree_Store,
     Pre    => not Is_Empty_Tree,
     Post   => (if  Right'Result/= Empty_Node then
                  In_Tree (Right'Result)),
     Inline;
   function Key (N : Valid_Tree_Node) return Key_Type with
     --  Global => Tree_Store,
     Pre    => not Is_Empty_Tree,
     Inline;

   function Value (N : Valid_Tree_Node) return Value_Type with
     --  Global Tree_Store,
     Pre => not Is_Empty_Tree,
     Inline;

   --  type Node_Abstraction is
   --     record
   --        Key   : Key_Type;
   --        Value : Value_Type;
   --        Level : Level_Type;
   --        Left  : Tree_Node;
   --        Right : Tree_Node;
   --     end record with Ghost;
   --
   --  Null_Node_Abstraction : constant Node_Abstraction := Node_Abstraction'
   --    (Key   => Null_Key,
   --     Value => Null_Value,
   --     Level => 0,
   --     Left  => Empty_Node,
   --     Right => Empty_Node) with
   --    Ghost;
   --
   --  type Tree_Abstraction is array (Valid_Tree_Node range <>) of Node_Abstraction
   --    with Ghost;
   --
   --  function Node_Contents (N : Tree_Node) return Node_Abstraction with
   --    Global => Tree_Store,
   --    Ghost;
   --
   --   function Tree_Contents_To (N : Tree_Node) return Tree_Abstraction with
   --    Global => Tree_Store,
   --    Post   =>
   --      Tree_Contents_To'Result'First = Tree_Node'First and
   --              Tree_Contents_To'Result'Last  = N and
   --              (for all Node in Tree_Contents_To'Result'Range =>
   --                 Tree_Contents_To'Result (Node) = Node_Contents (Node)),
   --    Ghost;
   --
   --  function Tree_Contents return Tree_Abstraction is
   --    (Tree_Contents_To (Last_Node_In_Tree))
   --     with
   --        Global => Tree_Store,
   --        Ghost;
   --
   --  function Preserved_Between (Tree_Before : Tree_Abstraction;
   --                              Tree_After  : Tree_Abstraction;
   --                              Except      : Tree_Node) return Boolean is
   --    (Tree_Before'First = Tree_After'First and then
   --     Tree_Before'Last = Tree_After'Last and then
   --       (if Except = Empty_Node
   --        then
   --           Tree_Before = Tree_After
   --        elsif Except = Tree_Before'First
   --        then
   --           Tree_Before (Except + 1 .. Tree_Before'Last) =
   --            Tree_After (Except + 1 .. Tree_After'Last)
   --        elsif Except = Tree_Before'Last
   --        then
   --           Tree_Before (Tree_Before'First .. Except - 1) =
   --            Tree_After (Tree_After'First.. Except - 1)
   --        else
   --           Except - 1 in Tree_Before'First .. Tree_Before'Last and then
   --           Except + 1 in Tree_Before'First .. Tree_Before'Last and then
   --           Tree_Before (Tree_Before'First .. Except - 1) =
   --            Tree_After (Tree_After'First .. Except - 1) and then
   --           Tree_Before (Except + 1 .. Tree_Before'Last) =
   --            Tree_After (Except + 1 .. Tree_After'Last)))
   --      with Ghost;

   procedure Set_Root (N : Valid_Tree_Node) with
     --  Global => (In_Out => Tree_Store),
     --  Post => Root = N and N in Valid_Tree_Node,
     Inline;

   procedure Set_Level (N : Valid_Tree_Node; Node_Level : Level_Type) with
     --  Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Level (N) = Node_Level and In_Tree (N),
           --  and
           --  Last_Node_In_Tree'Old = Last_Node_In_Tree and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After  => Tree_Contents,
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --     Tree_Contents'Old (N)'Update (Level => Node_Level),
     Inline;

   procedure Set_Left  (N : Valid_Tree_Node; Branch : Tree_Node) with
     --  Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Left (N) = Branch and In_Tree (N),
           --  and
           --  Last_Node_In_Tree'Old = Last_Node_In_Tree and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After  => Tree_Contents,
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --     Tree_Contents'Old (N)'Update (Left => Branch),
     Inline;

   procedure Set_Right (N : Valid_Tree_Node; Branch : Tree_Node) with
     --  Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Right (N) = Branch and In_Tree (N),
           --  and
           --  Last_Node_In_Tree'Old = Last_Node_In_Tree and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After  => Tree_Contents,
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --     Tree_Contents'Old (N)'Update (Right => Branch),
     Inline;

   procedure Set_Key (N : Valid_Tree_Node; The_Key : Key_Type) with
     --  Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Key (N) = The_Key and In_Tree (N),
           --  and
           --  Last_Node_In_Tree'Old = Last_Node_In_Tree and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After  => Tree_Contents,
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --     Tree_Contents'Old (N)'Update (Key => The_Key),
     Inline;
   procedure Set_Value (N : Valid_Tree_Node; Node_Value : Value_Type) with
     --  Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Value (N) = Node_Value and In_Tree (N),
           --  and
           --  Last_Node_In_Tree'Old = Last_Node_In_Tree and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After  => Tree_Contents,
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --     Tree_Contents'Old (N)'Update (Value => Node_Value),
     Inline;

   procedure Add_Node  (N : out Valid_Tree_Node; The_Key : Key_Type) with
     --  Global => (In_Out => Tree_Store),
     Post   => not Is_Empty_Tree and
               Key (N) = The_Key and In_Tree (N),
           --  and
           --  Preserved_Between (Tree_Before => Tree_Contents'Old,
           --                     Tree_After
           --                       => Tree_Contents_To (Last_Node_In_Tree),
           --                     Except      => Empty_Node) and
           --  Tree_Contents (N) =
           --      Null_Node_Abstraction'Update (Key => The_Key);
     Inline;

   procedure Clear_Tree_Below_Node (N : Valid_Tree_Node) with
     --  Global => (In_Out => Tree_Store),
     Pre  => not Is_Empty_Tree and then Is_A_Valid_Tree_Node (N),
     Post => not Is_Empty_Tree and
             Last_Node = N,
         --  and
         --  Tree_Contents'Last = N
         --  Preserved_Between (Tree_Before => Tree_Contents_To (N)'Old,
         --                       Tree_After  => Tree_Contents,
         --                       Except      => N) and
         --    Tree_Contents (N) =
         --       Tree_Contents'Old (N)'Update (Left => Empty_Node,
         --                                     Right => Empty_Node),
     Inline;
   --  Removes all nodes from the Tree with Tree_Node values > N.

private
   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Level_Type;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => 0,
      Left  => Empty_Node,
      Right => Empty_Node);

end Basic_Tree;
