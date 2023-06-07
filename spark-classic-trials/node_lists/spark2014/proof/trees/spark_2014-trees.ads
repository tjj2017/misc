with SPARK_2014.Dynamic_Tables;
generic
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Value : Value_Type;
package SPARK_2014.Trees is
   type Tree_Node is private;
   Empty_Node : constant Tree_Node;
   function Is_A_Node (N : Tree_Node) return Boolean
     with Ghost;

   type Tree_Type is private;

   function In_Tree  (T : Tree_Type; N : Tree_Node) return Boolean;
   function Node_Is_Present  (T : Tree_Type; N : Tree_Node) return Boolean;
   function Key_Is_Present (T : Tree_Type; K : Key_Type) return Boolean;
   function Persists (T_Pre, T_Post : Tree_Type) return Boolean
     with Ghost;

   procedure New_Tree (T : out Tree_Type);

   function Level (T : Tree_Type; N : Tree_Node) return Natural
     with Pre => In_Tree (T, N);
   function Left  (T : Tree_Type; N : Tree_Node) return Tree_Node
     with Pre  => In_Tree (T, N),
          Post => (if Is_A_Node (Left'Result)then In_Tree (T, Left'Result));
   function Right (T : Tree_Type; N : Tree_Node) return Tree_Node
     with Pre  => In_Tree (T, N),
          Post => (if Is_A_Node (Right'Result) then
                     In_Tree (T, Right'Result));
   function Key (T : Tree_Type; N : Tree_Node) return Key_Type
     with Pre => In_Tree (T, N);
   function Value (T : Tree_Type; N : Tree_Node) return Value_Type
     with Pre => In_Tree (T, N);

   procedure Set_Level (T : in out Tree_Type; N : Tree_Node;
                        Node_Level : Natural)
     with Pre  => In_Tree (T, N),
          Post => Persists (T'Old, T);
   procedure Set_Left  (T : in out Tree_Type; N : Tree_Node;
                        Branch : Tree_Node)
     with Pre  => In_Tree (T, N),
          Post => Persists (T'Old, T) and
                  (if In_Tree (T'Old, Branch) then In_Tree (T, Branch));
   procedure Set_Right (T : in out Tree_Type; N : Tree_Node;
                        Branch : Tree_Node)
     with Pre  => In_Tree (T, N),
          Post => Persists (T'Old, T) and
                  (if In_Tree (T'Old, Branch) then In_Tree (T, Branch));
   procedure Set_Key (T : in out Tree_Type; N : Tree_Node;
                      The_Key : Key_Type)
     with Pre  => In_Tree (T, N),
          Post => Persists (T'Old, T);
   procedure Set_Value (T : in out Tree_Type; N : Tree_Node;
                        Node_Value : Value_Type)
     with Pre  => In_Tree (T, N),
          Post => Persists (T'Old, T);
   procedure Add_Node  (T : in out Tree_Type; N : out Tree_Node;
                        The_Key : Key_Type)
     with Post => Persists (T'Old, T) and In_Tree (T, N) and
                  Key_Is_Present (T, The_Key);

   procedure Clear (T : in out Tree_Type; N : Tree_Node)
     with Post => not In_Tree (T, N);

private
   type Tree_Node is range 0 .. Natural'Last - 1;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := 0;

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Natural;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;

   package Dynamic_Tables is new SPARK_2014.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Valid_Tree_Node,
      Table_Low_Bound      => 1,
      Table_Initial        => 32,
      Table_Increment      => 100);

   type Tree_Type is
      record
         The_Tree : Dynamic_Tables.Table_Type;
      end record;

end SPARK_2014.Trees;
