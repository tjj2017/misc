---------------------------  Atrees_With_State  ------------------------------
--  This package implements an Anderson balanced tree. The tree is          --
--  maintained by the abstract state Tree_Store.  The package acts as a    --
--  template rather than as a geeric package as SPARK 2014 (at least the    --
--  2021 community version) does not handle generis with abstrate state     --
--  correctly.
--  The intended purpose of the Clear_Underlying_Tree_From_Node is to       --
--  provide a means for a tree to act like a stack. Nodes can be added to   --
--  the tree from a certain point and be "popped' off to that same point.   --
--  The function Last_Underlying_Node is called to identify the point to    --
--  where the nodes will be "popped" of from.                               --
--  The type Tree_Node, representing the nodes of the tree,                --
--  must have the range 0 .. Maximum_Number_Of_Nodes in the tree.          --
--  The Maximum_Number_Of_Nodes_In_Tree < Tree_Node'Base'Last.              --
--  Tree_Node'First represents the Empty_Node.                             --
--  Each node of the Tree has 5 fields:                                     --
--  Left, Right : Tree_Node;   --  the children of the node                 --
--  Level       : Level_Type;  --  the level of the node in the Tree        --
--  Key         : Key_Type;    --  place for key of node                    --
--  Value       : Value_Type;  --  place for value of node                  --
--  Both Key and Value need to have a null value provided.                  --
--  The values of these fields are set and interrogated by the subprograms  --
--  declared below.                                                         --
--  Procedure Init should be called to initialise the Tree_Store.          --                                     --
------------------------------------------------------------------------------
with Specific_Tree_Types;
with Bounded_Stacks;
use type Specific_Tree_Types.Tree_Node;

package Atrees_With_State
with SPARK_Mode,
     Abstract_State => Tree_Store
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

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   Stack_Size : Positive;

   function Empty_Tree return Boolean with
     Global => Tree_Store;

   function Tree_Depth return Natural with
     Global => Tree_Store,
     Pre => not Empty_Tree;

   function Is_Present (Key : Key_Type) return Boolean with
     Global => Tree_Store;

   function Value (Key : Key_Type) return Value_Type with
     Global => Tree_Store;

   procedure Init with
     Global => (Output => Tree_Store),
     Post   => Empty_Tree;

   procedure Insert (Key       : Key_Type;
                     Inserted  : out Boolean) with
     Global => (In_Out => Tree_Store),
     Post   => Is_Present (Key) and
               (Inserted = not Is_Present (Key)'Old);

   pragma Unevaluated_Use_Of_Old (Allow);
   procedure Insert_With_Value (Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type) with
     Global => (In_Out => Tree_Store),
     Post => Is_Present (Key) and
             Inserted = not Is_Present (Key)'Old and
             (if Inserted then
                 Value (Key) = Insert_Value and
                 Value_At_Node = Insert_Value
              else
                  Value (Key) = Value (Key)'Old and
                  Value_At_Node = Value (Key));

   procedure Update_Value (Key : Key_Type; New_Value : Value_Type) with
     Global => (In_Out => Tree_Store),
     Pre  => Is_Present (Key),
     Post => Value (Key) = New_Value;

   function Last_Node return Tree_Node with
     Global => Tree_Store;

   procedure Clear_Tree_Below_Node (Node : Valid_Tree_Node) with
     Global => (In_Out => Tree_Store);
   --  Removes all nodes below the given Node from the underlying tree.

   --  *************************************************************

   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   function New_Enumerator return Enumerator with
     Global => Tree_Store,
     Pre => not Empty_Tree;

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type);

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type);

private
   package Bounded_Stack is new
     Bounded_Stacks (Valid_Tree_Node, Stack_Size);

     type Enumerator is
      record
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stack.Stack;
      end record;

   type Direction is (Left, Right);

   function Get_Key (N : Tree_Node) return Key_Type with
     Global => Tree_Store,
     Pre  => N /= Empty_Node;

   procedure Find (Start_Node : Tree_Node;
                   Key        : Key_Type;
                   Found      : out Boolean;
                   Visited    : out Bounded_Stack.Stack) with
     Global => Tree_Store,
     Pre  => not Empty_Tree,
     Post => (if Found then
                (Get_Key (Bounded_Stack.Top (Visited)) = Key));
   --  Found is true ony if a node with the given Key is present.
   --  If Found, the top of the Visited stack is the Tree_Node
   --  in the tree which contains the Key.

   function Is_Present_From (Start_Node : Tree_Node;
                             Key        : Key_Type) return Boolean with
     Global => Tree_Store;

   function Value_From (Start_Node : Tree_Node; Key : Key_Type)
                        return Value_Type with
     Global => Tree_Store;

   procedure Insert_Value_From (Start_Node      : in out Tree_Node;
                                Key             : Key_Type;
                                Insert_Value    : Value_Type;
                                Overwrite       : Boolean;
                                Inserted        : out Boolean;
                                Insertion_Point : out Tree_Node;
                                Value_At_Node   : out Value_Type) with
     Global => (In_Out => Tree_Store),
     Post   => Is_Present_From (Start_Node, Key) and
     (Inserted = not Is_Present_From (Start_Node, Key)'Old) and
     (if Inserted or Overwrite then
        Insert_Value = Value_From (Start_Node, Key) and
          Value_At_Node = Insert_Value and
            Value (Insertion_Point) = Insert_Value
        else
          Value_At_Node = Value (Insertion_Point));

   function New_Enumerator_From (Start_Node : Valid_Tree_Node)
                                 return Enumerator;
end Atrees_With_State;
