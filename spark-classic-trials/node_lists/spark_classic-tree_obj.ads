with GNAT.Dynamic_Tables;
private package SPARK_Classic.Tree_Obj is
   type Tree is private;
   type Tree_Node is private;

   function Is_Valid (N : Tree_Node) return Boolean;

   function New_Tree return Tree;

   function Left (T : Tree; N : Tree_Node) return Tree_Node
   with Pre => Is_Valid (N);
   function Right (T : Tree; N : Tree_Node) return Tree_Node
   with Pre => Is_Valid (N);
   function Level (T : Tree; N : Tree_Node) return Natural;

   procedure New_Node (T : in out Tree; N : out Tree_Node)
   with Post => Is_Valid (N);
private
   type Tree_Node is range 0 .. Integer'Last;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;

   package Dynamic_Tree is new GNAT.Dynamic_Tables
     (Table_Component_Type => Tree,
      Table_Index_Type     => Valid_Tree_Node,
      Table_Low_Bound      => Valid_Tree_Node'First,
      Table_Initial        => 32,
      Table_Increment      => 100);

   type Tree is tagged
      record
