with SPARK_Classic.Symbols.Node_Tree,
     SPARK_Classic.Symbols.Node_Stacks;
use type SPARK_Classic.Symbols.Node_Tree.Tree_Node;
--  --# acquire Types;
package SPARK_Classic.Symbols.Nodes
--  --# own Store;
with Abstract_State => Store
is
   type Node_List is private;

   type Enumerator is private;

   procedure Initialize_Store
     with Global => (Output => Store),
          Post   => not Building_List;

   function Building_List return Boolean
   --  --# global Store;
   with Global => Store;

   function Empty_List (N_List : Node_List) return Boolean;

   procedure New_List (List : out Node_List)
   --  --# global in out Store;
   --  --# pre not Building_List;
   --  --# post Building_List and Empty_List (N_List);
   with Global => (In_Out => Store),
        Pre => not Building_List,
        Post => Building_List;

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List and not Empty_List (N_List);
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;

   procedure Insert_No_Duplicates (N : Types.Node_Id;
                                   List : in out Node_List;
                                   Inserted : out Boolean)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List and not Empty_List (N_List);

   procedure Insert_Allow_Duplicates (N : Types.Node_Id;
                                   List : in out Node_List)
   with Global => (In_Out => Store),
        Pre => Building_List,
        Post => Building_List;
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List and not Empty_List (N_List);

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

   function Tree_Depth (N_List : Node_List) return Natural
   --  -- global in Store;
     with Global => (Input => Store);

   function New_Enumerator (N_List : Node_List) return Enumerator
   --  --# global in Store;
   --  --# pre not Empty_List (N_List);
     with Global => (Input => Store),
          Pre    => not Empty_List (N_List);

   procedure Next (E : in out Enumerator; Next_Value: out Types.Node_Id)
   -- --#  global in Store;
   with Global => (Input => Store);

private
   type Node_List is
      record
         Root    : Node_Tree.Tree_Node;
         --  A stack to record visited nodes when inserting a new node.
         Visited : Node_Stacks.Stack_Type;
      end record;

   type Direction is (Left, Right);

   type Enumerator is
      record
         Root    : Node_List;
         Visited : Node_Stacks.Stack_Type;
      end record;
end SPARK_Classic.Symbols.Nodes;
