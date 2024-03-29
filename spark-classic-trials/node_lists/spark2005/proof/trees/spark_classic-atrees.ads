with SPARK_Classic.Trees,
     SPARK_Classic.Bounded_Stacks;
generic
   type Table_Component_Type is private;
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

package SPARK_Classic.Atrees is
   package Trees is new
     SPARK_Classic.Trees
       (Table_Component_Type => Table_Component_Type,
        Key_Type             => Key_Type,
        Value_Type           => Value_Type,
        Null_Value           => Null_Value);
   use type Trees.Tree_Node;

   subtype Tree_Node is Trees.Tree_Node;

   type A_Tree is tagged private;

   --  Null_A_Tree : constant A_Tree;

   function Empty_Tree (ATree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree)
      return Boolean;

   procedure New_A_Tree (ATree : out A_Tree; Tree_Container : Trees.Tree_Type)
     with Post => Empty_Tree (ATree);
   --  --# post Empty_Tree (ATree);

   function Count (ATree : A_Tree) return Natural;

   procedure Insert (ATree      : in out A_Tree;
                     Key        : Key_Type;
                     Inserted   : out Boolean)
     with Post => Inserted =
       (Count (ATree'Old) < Natural'Last) and then
       (Count (ATree) = Count (ATree'Old) + 1);
   --  --# post Inserted = Count (ATree) = Count (ATree~) + 1;

   procedure Insert_With_Value (ATree         : in out A_Tree;
                                Key           : Key_Type;
                                Value         : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
     with Post => Inserted =
       (Count (ATree'Old) < Natural'Last) and then
       (Count (ATree) = Count (ATree'Old) + 1);
   --  --# post Inserted = Count (ATree) = Count (ATree~) + 1;

   procedure Clear_A_Tree (ATree       : in out A_Tree)
     with Pre => not Empty_Tree (ATree),
          Post => Empty_Tree (ATree);
   --   --# Pre not Empty_Tree (ATree);
   --   --# post Empty_Tree (ATree);

   function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean;

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean;

   function Tree_Depth (ATree : A_Tree) return Natural;

   type Enumerator is private;

   function New_Enumerator (ATree : A_Tree) return Enumerator
     with Pre => not Empty_Tree (ATree);

   procedure Next_Node (E : in out Enumerator; Node : out Tree_Node);

private
   package Bounded_Stacks is new SPARK_Classic.Bounded_Stacks (Tree_Node, Stack_Size);

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stacks.Stack;
      end record;

   type A_Tree is tagged
      record
         Container : Trees.Tree_Type;
         Root      : Tree_Node;
         Count     : Natural;
      end record;

   --  Null_A_Tree : constant A_Tree := A_Tree'
   --    (Root  => Trees.Empty_Node,
   --     Count => 0);


   type Direction is (Left, Right);

end SPARK_Classic.Atrees;
