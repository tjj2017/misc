--------------------------------- Tree_With_State ----------------------------
--  This package is a template for a basic tree in which the abstract state --
--  of the package represents the nodes of the tree maintained by the       --
--  package.  The package utilises the generic package, Basic_Tree,         --
--  instantiated within the "hidden" (SPARK_Mode => Off) body of this       --
--  package.                                                                --
--  For practical SPARK analysis, this package has an explicit              --
--  abstract state, Tree_Store, declared.
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
--  Procedure Init should be called prior to using the tree structure to    --
--  set the tree to Not_Empty_Tree.                                         --
------------------------------------------------------------------------------
--  pragma Ada_2022;  --  Needed for the quantified expression.
with Specific_Tree_Types;
use type Specific_Tree_Types.Tree_Node;
private package Atrees_With_State.Tree_With_State with
SPARK_Mode,
  Abstract_State => (Tree_Store with Part_Of => Atrees_With_State.Tree_Store)
is
   package Withed_Tree_Types renames Specific_Tree_Types;

   subtype Tree_Node is Withed_Tree_Types.Tree_Node;   --  type range <>;
   subtype Level_Type is Withed_Tree_Types.Level_Type; --  type range <>;
   subtype Key_Type is Withed_Tree_Types.Key_Type;     --  type (<>);
   subtype Value_Type is Withed_Tree_Types.Value_Type; --  type private
   Null_Key : constant Key_Type := Withed_Tree_Types.Null_Key;
   Null_Value : constant Value_Type := Withed_Tree_Types.Null_Value;

   --  Valid_Tree_Node must exclude the Empty_Node, i.e.
   --  range Tree_Node'First + 1 .. Tree_Node'Last.
   subtype Valid_Tree_Node is Tree_Node range
     Tree_Node'First + 1 .. Tree_Node'Last;
   --  Empty_Node must equal Tree_Node'First
   Empty_Node : constant Tree_Node := Tree_Node'First;

   function Is_Empty_Tree return Boolean with
     Global => Tree_Store;

   function Base_Node return Tree_Node with
     Global => Tree_Store,
     Pre => Not Is_Empty_Tree;

   function Last_Node return Tree_Node with
     Global => Tree_Store;
   --  Returns the last node added to the tree - Empty_Node
   --  if the tree is empty.

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean with
     Global => Tree_Store,
     Post   => Is_A_Valid_Tree_Node'Result = (N in
                  Valid_Tree_Node'First .. Last_Node);

   function Root return Tree_Node with
     Global => Tree_Store;

   function In_Tree  (N : Tree_Node) return Boolean with
     Global => Tree_Store,
     Post   => (if In_Tree'Result then
                  N /= Empty_Node and Is_A_Valid_Tree_Node (N) and
                  N in Valid_Tree_Node);

   function Level (N : Valid_Tree_Node) return Level_Type with
     Global => Tree_Store,
     Pre    => not Is_Empty_Tree;

   function Left  (N : Valid_Tree_Node) return Tree_Node with
     Global => Tree_Store,
     Pre    => not Is_Empty_Tree,
     Post   => (if Left'Result /= Empty_Node then
                  In_Tree (Left'Result));

   function Right (N : Valid_Tree_Node) return Tree_Node with
     Global => Tree_Store,
     Pre    => not Is_Empty_Tree,
     Post   => (if Right'Result /= Empty_Node then
                  In_Tree (Right'Result));

   function Key (N : Valid_Tree_Node) return Key_Type with
     Global => Tree_Store,
     Pre    => not Is_Empty_Tree;

   function Value (N : Valid_Tree_Node) return Value_Type with
     Global => Tree_Store,
     Pre    => not Is_Empty_Tree;

   type Node_Abstraction is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Level_Type;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record with Ghost;

   Null_Node_Abstraction : constant Node_Abstraction := Node_Abstraction'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => 0,
      Left  => Empty_Node,
      Right => Empty_Node) with
     Ghost;

   type Tree_Abstraction is array (Valid_Tree_Node range <>) of Node_Abstraction
     with Ghost;

   function Node_Contents (N : Tree_Node) return Node_Abstraction with
     Global => Tree_Store,
     Ghost;

    function Tree_Contents_To (N : Tree_Node) return Tree_Abstraction with
     Global => Tree_Store,
     Post   =>
       Tree_Contents_To'Result'First = Tree_Node'First and
               Tree_Contents_To'Result'Last  = N and
               (for all Node in Tree_Contents_To'Result'Range =>
                  Tree_Contents_To'Result (Node) = Node_Contents (Node)),
     Ghost;

   function Tree_Contents return Tree_Abstraction is
     (Tree_Contents_To (Last_Node))
      with
         Global => Tree_Store,
         Ghost;

   function Preserved_Between (Tree_Before : Tree_Abstraction;
                               Tree_After  : Tree_Abstraction;
                               Except      : Tree_Node) return Boolean is
     (Tree_Before'First = Tree_After'First and then
      Tree_Before'Last = Tree_After'Last and then
        (if Except = Empty_Node
         then
            Tree_Before = Tree_After
         elsif Except = Tree_Before'First
         then
            Tree_Before (Except + 1 .. Tree_Before'Last) =
             Tree_After (Except + 1 .. Tree_After'Last)
         elsif Except = Tree_Before'Last
         then
            Tree_Before (Tree_Before'First .. Except - 1) =
             Tree_After (Tree_After'First.. Except - 1)
         else
            Except - 1 in Tree_Before'First .. Tree_Before'Last and then
            Except + 1 in Tree_Before'First .. Tree_Before'Last and then
            Tree_Before (Tree_Before'First .. Except - 1) =
             Tree_After (Tree_After'First .. Except - 1) and then
            Tree_Before (Except + 1 .. Tree_Before'Last) =
             Tree_After (Except + 1 .. Tree_After'Last)))
       with Ghost;

   procedure Init with
     Global => (Output => Tree_Store),
     Post   => Is_Empty_Tree;

   procedure Set_Root (N : Valid_Tree_Node) with
     Global => (In_Out => Tree_Store),
     Post => Root = N and Root in Valid_Tree_Node;

   procedure Set_Level (N : Valid_Tree_Node; Node_Level : Level_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Level (N) = Node_Level and In_Tree (N) and
               Last_Node'Old = Last_Node and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After  => Tree_Contents,
                                  Except      => Empty_Node) and
                  Tree_Contents (N) =
                    Tree_Contents'Old (N)'Update (Level => Node_Level);

   procedure Set_Left  (N : Valid_Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Left (N) = Branch and In_Tree (N)  and
               Last_Node = Last_Node and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After  => Tree_Contents,
                                  Except      => Empty_Node) and
                  Tree_Contents (N) =
                    Tree_Contents'Old (N)'Update (Left => Branch);

 procedure Set_Right (N : Valid_Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Right (N) = Branch and In_Tree (N)  and
               Last_Node'Old = Last_Node and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After  => Tree_Contents,
                                  Except      => Empty_Node) and
                  Tree_Contents (N) =
                    Tree_Contents'Old (N)'Update (Right => Branch);

   procedure Set_Key (N : Valid_Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Key (N) = The_Key and In_Tree (N) and
               Last_Node'Old = Last_Node and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After  => Tree_Contents,
                                  Except      => N) and
                  Tree_Contents (N) =
                    Tree_Contents'Old (N)'Update (Key => The_Key);

   procedure Set_Value (N : Valid_Tree_Node; Node_Value : Value_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => not Is_Empty_Tree,
     Post   => Value (N) = Node_Value and In_Tree (N) and
               Last_Node'Old = Last_Node and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After  => Tree_Contents,
                                  Except      => N) and
                  Tree_Contents (N) =
                    Tree_Contents'Old (N)'Update (Value => Node_Value);

   procedure Add_Node  (N : out Valid_Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Post   => not Is_Empty_Tree and
               Key (N) = The_Key and In_Tree (N) and
               Last_Node = Last_Node'Old + 1 and
               Preserved_Between (Tree_Before => Tree_Contents'Old,
                                  Tree_After
                                  => Tree_Contents_To (Last_Node),
                                  Except      => Empty_Node) and
              Tree_Contents (N) =
                 Null_Node_Abstraction'Update (Key => The_Key);

   procedure Clear_Tree_Below_Node (N : Valid_Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre  => not Is_Empty_Tree and then Is_A_Valid_Tree_Node (N),
     Post => not Is_Empty_Tree and
             Last_Node = N and
             Tree_Contents'Last = N and
             Preserved_Between (Tree_Before => Tree_Contents_To (N)'Old,
                                Tree_After  => Tree_Contents,
                                Except      => N) and
             Tree_Contents (N) =
                Tree_Contents'Old (N)'Update (Left => Empty_Node,
                                              Right => Empty_Node);
   --  Removes all nodes from the Tree with Tree_Node values > N.

end Atrees_With_State.Tree_With_State;
