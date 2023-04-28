with SPARK_2014.Trees,
     SPARK_2014.Bounded_Stacks;
generic
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Value : Value_Type;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   Stack_Size : Positive;

package SPARK_2014.Atrees
with SPARK_Mode
is
   package Trees is new
     SPARK_2014.Trees
       (Key_Type             => Key_Type,
        Value_Type           => Value_Type,
        Null_Value           => Null_Value);
   use type Trees.Tree_Node;

   subtype Tree_Node is Trees.Tree_Node;

   type A_Tree is private;

   function Empty_Tree (ATree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree)
      return Boolean;

   procedure New_A_Tree (ATree : out A_Tree; Tree_Container : Trees.Tree_Type)
     with Post => Empty_Tree (ATree);

   function Count (ATree : A_Tree) return Natural;

   procedure Insert (ATree      : in out A_Tree;
                     Key        : Key_Type;
                     Inserted   : out Boolean)
     with Post => (if Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Insert_With_Value (ATree         : in out A_Tree;
                                Key           : Key_Type;
                                Value         : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
     with Pre  => Count (ATree) < Natural'Last,
          Post => (if Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Clear_A_Tree (ATree       : in out A_Tree)
     with Pre => not Empty_Tree (ATree),
          Post => Empty_Tree (ATree);

   function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean;

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean
   with Pre => Populated (ATree);

   function Tree_Depth (ATree : A_Tree) return Natural;

   type Enumerator is private;

   function New_Enumerator (ATree : A_Tree) return Enumerator
     with Pre => not Empty_Tree (ATree);

   procedure Next_Node (E : in out Enumerator; Node : out Tree_Node);

private
   package Bounded_Stacks is new SPARK_2014.Bounded_Stacks (Tree_Node, Stack_Size);

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stacks.Stack;
      end record;

   type A_Tree is
      record
         Container : Trees.Tree_Type;
         Root      : Tree_Node;
         Count     : Natural;
      end record;

   type Direction is (Left, Right);

end SPARK_2014.Atrees;
