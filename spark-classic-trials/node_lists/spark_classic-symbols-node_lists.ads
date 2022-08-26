with SPARK_Classic.Trees,
     SPARK_Classic.Stacks;
private package SPARK_Classic.Symbols.Node_Lists
--  --# own Store;
with Abstract_State => Store
is
   type Node_List is private;

   type Enumerator is limited private;

   procedure Initialize_Store
     with Global => (Output => Store),
          Post   => not Building_List;

   function Building_List return Boolean
   --  --# global Store;
   with Global => Store;

   procedure New_List (List : out Node_List)
   --  --# global in out Store;
   --  --# pre not Building_List;
   --  --# post Building_List;
   with Global => (In_Out => Store),
        Pre => not Building_List,
        Post => Building_List;

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List;
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;

   procedure Save_List (List : in out Node_List)
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post not Building_List;
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => not Building_List;

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
   --  --# global in Store;
     with Global => (Input => Store);

   function New_Enumerator (N_List : Node_List) return Enumerator
   --  --# global in Store;
     with Global => (Input => Store);

   function Next (E : Enumerator) return Types.Node_Id
   -- --#  global in Store;
   with Global => (Input => Store);

private
   package A_Tree is new Trees.Tree
     (Element_Type => Types.Node_Id);
   use type A_Tree.Tree_Node;

   type Node_List is
      record
         Root : A_Tree.Tree_Node;
      end record;

   type Direction is (Left, Right);

   package Dynamic_Stack is new SPARK_Classic.Stacks.Stack
     (Element_Type => A_Tree.Tree_Node);

   type Enumerator is
      record
         Root    : Node_List;
         Place   : A_Tree.Tree_Node;
         Visited : Dynamic_Stack.Stack_Type;
         Dir     : Direction;
      end record;
end SPARK_Classic.Symbols.Node_Lists;
