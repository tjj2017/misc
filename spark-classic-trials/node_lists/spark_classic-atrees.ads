with SPARK_Classic.Trees,
     SPARK_Classic.Stacks;
generic
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Value : Value_Type;

   with package Trees is new
     SPARK_Classic.Trees (Key_Type, Value_Type, Null_Value);

package SPARK_Classic.Atrees is
   type A_Tree is tagged private;

   function Empty_Tree (Tree : A_Tree) return Boolean;

   procedure New_Tree (Tree : out A_Tree)
     with Post => Empty_Tree (Tree);
   --  --# post Empty_Tree (Tree);

   procedure Insert (Tree       : in out A_Tree;
                     Key        : Key_Type;
                     Tree_Store : in out Trees.Tree_Type;
                     Inserted   : out Boolean)
     with Post => Empty_Tree (Tree) = Inserted or Empty_Tree (Tree'Old);
   --  --# post Empty_Tree (Tree) = Inserted or Empty_Tree (Tree~);

   function Is_Present (Tree       : A_Tree;
                        Key        : Key_Type;
                        Tree_Store : Trees.Tree_Type) return Boolean;

   function Tree_Depth (Tree       : A_Tree;
                        Tree_Store : Trees.Tree_Type) return Natural;

   type Enumerator is private;

   function New_Enumerator (Tree       : A_Tree;
                            Tree_Store : Trees.Tree_Type) return Enumerator
     with Pre => not Empty_Tree (Tree);

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type);

   procedure Next_Value (E : in out Enumerator; Value: out Value_Type);
private
   package Stacks is new SPARK_Classic.Stacks (Trees.Tree_Node);

   type A_Tree is tagged
      record
         Root    : Trees.Tree_Node;
         --  A stack to record visited nodes when inserting a new node.
         Visited : Stacks.Stack_Type;
      end record;

   type Direction is (Left, Right);

   type Enumerator is
      record
         Root    : A_Tree;
         Visited : Stacks.Stack_Type;
      end record;
end SPARK_Classic.Atrees;
