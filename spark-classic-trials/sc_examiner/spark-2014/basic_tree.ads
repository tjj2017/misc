---------------------------------- Basic_Tree --------------------------------
--  This package is a stateless abstraction providing basic operations on   --
--  an underlying tree.  The body of this package must be excluded from SPARK  --
--  analysis because of the hidden state.  The stateless view of the        --
--  package overcomes limitations of the 2021 SPARK-2012 examiner handling  --
--  state_abstraction in generic packages and the inability to "hide"       --
--  private pointer types.                                                  --
--  For practical SPARK analysis a version of this package with an explicit --
--  abstract state is required.  The intention is to define an equvalent    --
--  package, the declaration of which has the explicit abstract state named --
--  and its body is "hidden" package body (SPARK_Mode => Off) and has an    --
--  instantiation of this generic package. The individual subprograms of    --
--  instatiated package are renamed to form the bodies of the subprograms   --
--  declared in the package with explicit abstract state.                   --
--                                                                          --
--  To assist in writing the package declaration of the package with the    --
--  explicit abstract state, the package declaration of this generic package--
--  contains the abstract state declaration and the global, pre and post    --
--  definitions using the abstract state provided but commented out.        --
--                                                                          --
--  Nodes are added to the tree using Add_Node and each node added has a    --
--  monotonically increased value to the previous node added.                --
--  The type Tree_Node, representing the nodes of the tree,                 --
--  must have the range 0 .. Maximum_Number_Of_Nodes in the tree.           --
--  The Maximum_Number_Of_Nodes_In_Tree < Tree_Node'Base'Last.              --
--  Tree_Node'First represents the Empty_Node.                              --
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
generic
   type Tree_Node is range <>;

   type Level_Type is range <>;
   type Key_Type is private;
   type Value_Type is private;
   Null_Key : Key_Type;
   Null_Value : Value_Type;

package Basic_Tree with
SPARK_Mode
  -- Abstract_State => Tree_Store
is
   procedure Init;

   --  Empty_Node must equal Tree_Node'First
   Empty_Node : constant Tree_Node := Tree_Node'First;
   --  Valid_Tree_Node must exclude the Empty_Node, i.e.
   --  range Tree_Node'First + 1 .. Tree_Node'Last.
   subtype Valid_Tree_Node is Tree_Node range
     Tree_Node'First + 1 .. Tree_Node'Last;

   function Is_Empty_Tree return Boolean with
     --  Global => Tree_Store,
     Inline;

   function First_Node_In_Tree return Tree_Node is (Valid_Tree_Node'First) with
     --  Global => Tree_Store,
     Inline;

   function Last_Node_In_Tree return Tree_Node with
     --  Global => Tree_Store,
     Inline;
   --  Returns the last node added to the tree - Enpty_Node
   --  if tree is empty.

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean with
     --  Global => Tree_Store,
     Inline;

   function In_Tree  (N : Tree_Node) return Boolean with
     -- Global => Tree_Store,
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

   procedure Clear_Tree_Below_Node (N : Tree_Node) with
     --  Global => (In_Out => Tree_Store),
     Pre  => not Is_Empty_Tree and then Is_A_Valid_Tree_Node (N),
     Post => not Is_Empty_Tree and
             Last_Node_In_Tree = N,
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
