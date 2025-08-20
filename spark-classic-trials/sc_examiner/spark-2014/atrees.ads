----------------------------------  Atrees  ----------------------------------
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
--  object but be aware of using the Clear_Underlying_Tree_From subprogram  --
--  as this is not A_Tree object aware and will clear from the given        --
--  Atree root the rest of the underlying tree including any other Atrees   --
--  contained within  the cleared part of the underlying tree.              --
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
with Tree_Abstraction;
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

package Atrees
with SPARK_Mode
is
   type A_Tree is private;

   function Empty_Tree (ATree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree) return Boolean;

   function Count (ATree : A_Tree) return Natural;

   function Tree_Depth (ATree : A_Tree) return Natural
     with Pre => Populated (ATree);

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean;

   function Value (ATree : A_Tree; Key : Key_Type) return Value_Type;

   function Equal_Keys (ATree_1, ATree_2 : A_Tree) return Boolean
   with Pre => Populated (ATree_1) and Populated (ATree_2);

   function Equal_Keys_And_Values (ATree_1, ATree_2 : A_Tree) return Boolean
   with Pre => Populated (ATree_1) and Populated (ATree_2);

   procedure New_A_Tree (Tree : out A_Tree)
     with Post => Empty_Tree (Tree);

   procedure Insert (Tree      : in out A_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean)
     with Pre  => Count (Tree) < Natural'Last,
     Post => Is_Present (Tree, Key) and
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

   procedure Clear_Underlying_Tree_From (Tree : in out A_Tree)
     with Pre => not Empty_Tree (Tree),
          Post => Empty_Tree (Tree);

   type Enumerator is private;

   function New_Enumerator (ATree : A_Tree) return Enumerator
     with Pre => Populated (ATree);

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type);

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
      end record;

   type Direction is (Left, Right);

end Atrees;
