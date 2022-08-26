with GNAT.Dynamic_Tables;
private package SPARK_Classic.Trees is
   generic
      type Element_Type is private;
   package Tree is
      type Tree_Type is private;
      type Tree_Node is private;

      function Is_Valid (N : Tree_Node) return Boolean;
      function Present  (N : Tree_Node) return Boolean;

      function New_Tree return Tree_Type;

      function Level (T : Tree_Type; N : Tree_Node) return Natural;
      function Left  (T : Tree_Type; N : Tree_Node) return Tree_Node
        with Pre => Is_Valid (N);
      function Right (T : Tree_Type; N : Tree_Node) return Tree_Node
        with Pre => Is_Valid (N);
      function Value (T : Tree_Type; N : Tree_Node) return Element_Type
        with Pre => Is_Valid (N);

      procedure Set_Level (T : in out Tree_Type; N : Tree_Node;
                           Node_Level : Natural)
        with Pre => Is_Valid (N);
      procedure Set_Left  (T : in out Tree_Type; N : Tree_Node;
                           Branch : Tree_Node)
        with Pre => Is_Valid (N);
      procedure Set_Right (T : in out Tree_Type; N : Tree_Node;
                           Branch : Tree_Node)
        with Pre => Is_Valid (N);
      procedure Set_Value (T : in out Tree_Type; N : Tree_Node;
                          Node_Value : Element_Type)
        with Pre => Is_Valid (N);
      procedure Add_Node  (T : in out Tree_Type; N : out Tree_Node;
                           V : Element_Type)
        with Post => Is_Valid (N);
      procedure Clear (T : in out Tree_Type; N : Tree_Node);
   private
      type Tree_Node is range 0 .. Natural'Last - 1;
      subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
      Empty_Node : constant Tree_Node := 0;

      type Actual_Node is
         record
            Value : Element_Type;
            Level : Natural;
            Left  : Tree_Node;
            Right : Tree_Node;
         end record;

      package Dynamic_Tree is new GNAT.Dynamic_Tables
        (Table_Component_Type => Actual_Node,
         Table_Index_Type     => Valid_Tree_Node,
         Table_Low_Bound      => 1,
         Table_Initial        => 32,
         Table_Increment      => 100);

      type Tree_Type is
         record
            T_Node   : Tree_Node;
            The_Tree : Dynamic_Tree.Instance;
         end record;

   end Tree;

end SPARK_Classic.Trees;
