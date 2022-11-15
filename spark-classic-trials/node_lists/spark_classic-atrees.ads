with SPARK_Classic.Trees,
     SPARK_Classic.Bounded_Stacks;
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

package SPARK_Classic.Atrees is
   package Trees is new
     SPARK_Classic.Trees (Key_Type, Value_Type, Null_Value);
   use type Trees.Tree_Node;

   subtype Tree_Type is Trees.Tree_Type;
   subtype Tree_Node is Trees.Tree_Node;

   type A_Tree is tagged private;

   Null_A_Tree : constant A_Tree;

   function Empty_Tree (Tree : A_Tree) return Boolean;

   procedure New_Tree (Tree : out A_Tree)
     with Post => Empty_Tree (Tree);
   --  --# post Empty_Tree (Tree);

   function Count (Tree : A_Tree) return Natural;

   procedure Insert (Tree       : in out A_Tree;
                     Key        : Key_Type;
                     Tree_Store : in out Tree_Type;
                     Inserted   : out Boolean)
     with Post => Inserted =
       (Count (Tree'Old) < Natural'Last) and then
       (Count (Tree) = Count (Tree'Old) + 1);
   --  --# post Inserted = Count (Tree) = Count (Tree~) + 1;

   procedure Insert_With_Value (Tree          : in out A_Tree;
                                Key           : Key_Type;
                                Value         : Value_Type;
                                Tree_Store    : in out Tree_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
     with Post => Inserted =
       (Count (Tree'Old) < Natural'Last) and then
       (Count (Tree) = Count (Tree'Old) + 1);
   --  --# post Inserted = Count (Tree) = Count (Tree~) + 1;

   procedure Clear (Tree       : in out A_Tree;
                    Tree_Store : in out Tree_Type)
     with Pre => not Empty_Tree (Tree),
          Post => Empty_Tree (Tree);
   --   --# Pre not Empty_Tree (Tree);
   --   --# post Empty_Tree (Tree);

   function Is_Equal (Tree_1       : A_Tree;
                      Tree_2       : A_Tree;
                      Tree_Store_1 : Tree_Type;
                      Tree_Store_2 : Tree_Type) return Boolean;

   function Is_Present (Tree       : A_Tree;
                        Key        : Key_Type;
                        Tree_Store : Tree_Type) return Boolean;

   function Tree_Depth (Tree       : A_Tree;
                        Tree_Store : Tree_Type) return Natural;

   type Enumerator is private;

   function New_Enumerator (Tree       : A_Tree;
                            Tree_Store : Tree_Type) return Enumerator
     with Pre => not Empty_Tree (Tree);

   procedure Next_Node (E : in out Enumerator; Tree_Store : Tree_Type;
                        Node : out Tree_Node);

private
   package Stacks is new SPARK_Classic.Bounded_Stacks (Tree_Node, Stack_Size);

   type Enumerator is
      record
         Root    : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Stacks.Stack;
      end record;

   type A_Tree is tagged
      record
         Root    : Tree_Node;
         Count   : Natural;
      end record;

   Null_A_Tree : constant A_Tree := A_Tree'
     (Root  => Trees.Empty_Node,
      Count => 0);


   type Direction is (Left, Right);

end SPARK_Classic.Atrees;
