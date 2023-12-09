with SPARK_2014.Dynamic_Tables;
generic
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Value : Value_Type;
package SPARK_2014.Trees with
  Abstract_State => Tree_Store,
  Initializes    => Tree_Store
is
   type Tree_Node is private;
   Empty_Node : constant Tree_Node;
   function Is_A_Node (N : Tree_Node) return Boolean
     with Ghost;

   type Tree_Type is private with Ghost;
   function TS return Tree_Type with
     Global => Tree_Store,
     Ghost;

   function In_Tree  (N : Tree_Node) return Boolean with
     Global => Tree_Store;
   function Key_Is_Present (K : Key_Type) return Boolean with
     Global => Tree_Store;
   function Node_Is_Present  (N : Tree_Node) return Boolean with
     Global => Tree_Store;
   function Persists (T_Pre, T_Post : Tree_Type) return Boolean with
     Global => Tree_Store,
     Ghost;

   function Level (N : Tree_Node) return Natural with
     Global => Tree_Store,
     Pre    => In_Tree (N);
   function Left  (N : Tree_Node) return Tree_Node with
     Global => Tree_Store,
     Pre    => In_Tree (N),
     Post   => (if Is_A_Node (Left'Result)then In_Tree (Left'Result));
   function Right (N : Tree_Node) return Tree_Node with
     Global => Tree_Store,
     Pre    => In_Tree (N),
     Post   => (if Is_A_Node (Right'Result) then In_Tree (Right'Result));
   function Key (N : Tree_Node) return Key_Type with
     Global => Tree_Store,
     Pre    => In_Tree (N);
   function Value (N : Tree_Node) return Value_Type with
     Global => Tree_Store,
     Pre    => In_Tree (N);

   procedure Set_Level (N : Tree_Node; Node_Level : Natural) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N),
     Post   => Persists (TS'Old, TS) and Level (N) = Node_Level;
   procedure Set_Left  (N : Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N),
     Post   => Persists (TS'Old, TS) and
                  (if In_Tree (Branch)'Old then In_Tree (Branch));
   procedure Set_Right (N : Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N),
     Post   => Persists (TS'Old, TS) and
                  (if In_Tree (Branch)'Old then In_Tree (Branch));
   procedure Set_Key (N : Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N),
     Post   => Persists (TS'Old, TS) and Key_Is_Present (The_Key);
   procedure Set_Value (N : Tree_Node; Node_Value : Value_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N),
     Post   => Persists (TS'Old, TS);
   procedure Add_Node  (N : out Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Post   => Persists (TS'Old, TS) and In_Tree (N) and
                  Key_Is_Present (The_Key);

   procedure Clear (N : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Post   => not In_Tree (N);

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

   type Actual_Tree_Type is
      record
         The_Tree : Dynamic_Tables.Table_Type;
      end record;

   type Tree_Type is new Actual_Tree_Type;

end SPARK_2014.Trees;
