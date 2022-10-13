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

   function Empty_Tree (Tree : A_Tree;
                        Tree_Store : Tree_Type) return Boolean;

   procedure New_Tree (Tree : out A_Tree;
                      Tree_Store : Tree_Type)
     with Post => Empty_Tree (Tree, Tree_Store);
   --  --# post Empty_Tree (Tree, Tree_Store);

   procedure Insert (Tree       : in out A_Tree;
                     Key        : Key_Type;
                     Tree_Store : in out Tree_Type;
                     Inserted   : out Boolean)
     with Post => Empty_Tree (Tree, Tree_Store) = not Inserted and
                              Empty_Tree (Tree'Old, Tree_Store);
   --  --# post Empty_Tree (Tree, Tree_Store) = not Inserted and
   --  --#                  Empty_Tree (Tree~, Tree_Store);

   function Is_Equal (Tree_1       : A_Tree;
                      Tree_2       : A_Tree;
                      Tree_Store_1 : Tree_Type;
                      Tree_Store_2 : Tree_Type) return Boolean;

   function Is_Present (Tree       : A_Tree;
                        Key        : Key_Type;
                        Tree_Store : Tree_Type) return Boolean;

   function Tree_Depth (Tree       : A_Tree;
                        Tree_Store : Tree_Type) return Natural;

   function Count (Tree : A_Tree) return Natural;

   type Enumerator is private;

   function New_Enumerator (Tree       : A_Tree;
                            Tree_Store : Tree_Type) return Enumerator
     with Pre => not Empty_Tree (Tree, Tree_Store);

   procedure Next_Node (E : in out Enumerator; Tree_Store : Tree_Type;
                        Node : out Tree_Node);

private
   package Stacks is new SPARK_Classic.Bounded_Stacks (Tree_Node, Stack_Size);

   type A_Tree is tagged
      record
         Root    : Tree_Node;
         Count   : Natural;
      end record;

   type Direction is (Left, Right);

   type Enumerator is
      record
         Root    : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Stacks.Stack;
      end record;
end SPARK_Classic.Atrees;
