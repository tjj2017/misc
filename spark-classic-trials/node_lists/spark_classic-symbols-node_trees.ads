with SPARK_Classic.Atrees;
package SPARK_Classic.Symbols.Node_Trees
with Abstract_State => Store
--  --# own Store;
is
   type Node_List is tagged private;

   type Enumerator is private;

   procedure Initialize_Store
     with Global => (Output => Store),
          Post   => not Building_List;

   function Building_List return Boolean
   with Global => Store;
   --  --# global Store;

   function Empty_List (List : Node_List) return Boolean;

   procedure New_List (List : out Node_List)
   with Global => (In_Out => Store),
        Pre => not Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre not Building_List;
   --  --# post Building_List and Empty_List (N_List);

   procedure Insert (N : Types.Node_Id;
                     List : in out Node_List;
                     Inserted : out Boolean)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List and not Empty_List (N_List);

   procedure Save_List (List : in out Node_List)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => not Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post not Building_List;

   function Are_Equal (List_1, List_2 : Node_List) return Boolean
     with Global => (Input => Store);
   --  --# global in Store;

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
     with Global => (Input => Store);
   --  --# global in Store;

   function Tree_Depth (N_List : Node_List) return Natural
     with Global => (Input => Store);
   --  -- global in Store;

   function Count (N_list : Node_List) return Natural;

   function New_Enumerator (N_List : Node_List) return Enumerator
     with Global => (Input => Store),
          Pre    => not Empty_List (N_List);
   --  --# global in Store;
   --  --# pre not Empty_List (N_List);

   procedure Next (E : in out Enumerator; Next_Value: out Types.Node_Id)
   with Global => (Input => Store);
   -- --#  global in Store;

private
   type Null_Record is null record;
   Null_Value : constant Null_Record := (null record);

   --  A stack is need by Atrees to record the visited nodes of the tree.
   --  The stack size needed is dependent on the height of the tree.
   --  For a balanced binary tree the height of a tree, k, with N nodes
   --  is given by
   --  k = log2 (N + 1) - 1.
   --  As the tree can have no duplicates, the maximum number of nodes is
   --  Types.Node_Id'Last - Types.Node_Id'Last + 1 = 100000000
   --  So, the maximum tree height is
   --  Log2 (100000000 + 1) - 1 = 25.58
   --  The stack size does not need to be more than a couple extra elements
   --  above the tree height so making it 32 gives a good margin.
   package Atrees is new SPARK_Classic.Atrees
     (Key_Type   => Types.Node_Id,
      Value_Type => Null_Record,
      Null_Value => Null_Value,
      Stack_Size => 32);

   type Node_List is tagged record
      Tree   : Atrees.A_Tree;
      Length : Natural;
   end record;

   type Enumerator is record
      E : Atrees.Enumerator;
   end record;

end SPARK_Classic.Symbols.Node_Trees;
