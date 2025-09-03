---------------------------  Atrees_With_State  ------------------------------
--  This package is a stateless abstraction of an Anderson balanced tree    --
--  implementation. The Atree is represented by its collection of           --
--  Atree_Nodes, which are also the embodiment of the state hidden within   --
--  the instantiation of the Tree_Abstraction in the package body of Atrees.--
--  The stateless view of the package overcomes limitations of the          --
--  2021 SPARK-2012 examiner handling state_abstraction in generic packages.--
--  The use of type A_Tree must be restricted.  Although, in principle,     --
--  multiple declarations of A_Tree objects is possible, all the objects    --
--  all use the same hidden state of the underlying Tree_Abstraction.       --
--  It is possible to have multiple A_Trees using the same underlying tree  --
--  object but be aware of the Clear_Underlying_Tree_From_Node subprogram   --
--  as this is not A_Tree object aware and will clear from the given        --
--  Atree root the rest of the underlying tree including any other Atrees   --
--  contained within  the cleared part of the underlying tree.              --
--  The intended purpose of the Clear_Underlying_Tree_From_Node is to       --
--  provide a means for a tree to act like a stack. Nodes can be added to   --
--  the tree from a certain point and be "popped' off to that same point.   --
--  The function Last_Underlying_Node is called to identify the point to    --
--  where the nodes will be "popped" of from.                               --
--  The type Atree_Node, representing the nodes of the tree,                --
--  must have the range 0 .. Maximum_Number_Of_Nodes in the Atree.          --
--  The Maximum_Number_Of_Nodes_In_Tree < Aree_Node'Base'Last.              --
--  Atree_Node'First represents the Empty_Node.                             --
--  Each node of the Tree has 5 fields:                                     --
--  Left, Right : Tree_Node;   --  the children of the node                 --
--  Level       : Level_Type;  --  the level of the node in the Tree        --
--  Key         : Key_Type;    --  place for key of node                    --
--  Value       : Value_Type;  --  place for value of node                  --
--  Both Key and Value need to have a null value provided.                  --
--  The values of these fields are set and interrogated by the subprograms  --
--  declared below.                                                         --
--  Procedure Init should be called to initialise the underlying tree used  --
--  to store the A_Tree objects.                                            --
------------------------------------------------------------------------------
with Tree_With_State;
with Bounded_Stacks;
generic
   type Atree_Node is  range <>;
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Key : Key_Type;
   Null_Value : Value_Type;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   Stack_Size : Positive;

package Atrees_With_State
with SPARK_Mode,
     Abstract_State => Atree_Store
is
   function Empty_Tree return Boolean with
     Global => Atree_Store;

   function Populated return Boolean with
     Global => Atree_Store;

   function Count return Natural with
     Global => Atree_Store;

   function Tree_Depth return Natural with
     Global => Atree_Store,
     Pre => Populated;

   function Is_Present (Key : Key_Type) return Boolean with
     Global => Atree_Store;

   function Value (Key : Key_Type) return Value_Type with
     Global => Atree_Store;

   procedure Init with
     Global => (Output => Atree_Store),
     Post   => Empty_Tree;

   procedure Insert (Key       : Key_Type;
                     Inserted  : out Boolean) with
     Global => Atree_Store,
     Pre    => Count < Natural'Last,
     Post   => Is_Present (Key) and
             (if not Populated (Tree'Old) then
                Count (Tree) = 1
              elsif Inserted then
                Count (Tree) = Count (Tree'Old) + 1
               else
                Count (Tree) = Count (Tree'Old));

   procedure Insert_With_Value (Tree          : in out A_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
     with Pre  => Count (Tree) < Natural'Last,
     Post => Is_Present (Tree, Key) and
             (if Inserted then Value (Tree, Key) = Insert_Value) and
             (if not Populated (Tree'Old) then
                Count (Tree) = 1
                  elsif Inserted then
                     Count (Tree) = Count (Tree'Old) + 1
                   else
                  Count (Tree) = Count (Tree'Old));

   --  *** The following subprograms should be used with care. ***
   --  *** They operate on the underlying tree structure and   ***
   --  *** are not A_Tree object aware.                        ***

   function Last_Underlying_Tree_Node (Dummy : Atree_Node) return Atree_Node;
   --  Returns the last used node in the underlying tree. Each successful
   --  insertion (subprogram Insert parameter Inserted = True) creates a
   --  new node in the underlying tree. The Last_Underlying_Tree_Node will
   --  be the one created from the last successful Insert.

   procedure Clear_Underlying_Tree_From_Node (Node : in out Atree_Node);
   --  Removes all nodes below the given Node from the underlying tree.
   --  This will invalidate all A_Tree objects

   --  *************************************************************

   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   function New_Enumerator (ATree : A_Tree) return Enumerator
     with Pre => Populated (ATree);

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type);

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type);

private
   Empty_Node : constant Atree_Node := Atree_Node'First;

   package Bounded_Stack is new
     Bounded_Stacks (Atree_Node, Stack_Size);

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stack.Stack;
      end record;

   type A_Tree is
      record
         Root      : Atree_Node;
         Count     : Natural;
         Toggle    : Boolean;
      end record;

   type Direction is (Left, Right);

end Atrees_With_State;
