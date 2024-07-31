----------------------------  SPARK_2014.Tree  -------------------------------
--  Modified sPARK_2014.Trees package to make it a private child of         --
--  SPARK_2014.Tree_Abs, a thin interface giving a stateless abstraction.   --
------------------------------------------------------------------------------
private package SPARK_2014.Multi_Atree.Tree_Abs.Tree with
  SPARK_Mode => Off,
  Abstract_State => Tree_Store,
  Initializes    => Tree_Store
is
--   type Tree_Node is private;
--   Empty_Node : constant Tree_Node;
   function Is_A_Node (N : Tree_Node) return Boolean
     with Ghost;

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean with
     Global => Tree_Store;

   subtype Key_Type is Natural;
   subtype Valid_Key_Type is Key_Type range
     Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   subtype Value_Type is Integer;

   Null_Key : constant Key_Type := Key_Type'First;
   Null_Value : constant Value_Type := 0;

   type Tree_Type is private with Ghost;
   function TS return Tree_Type with
     Global => Tree_Store,
     Ghost;

   function Is_Empty return Boolean with
     Global => Tree_Store;

   function In_Tree  (N : Tree_Node) return Boolean with
     Global => Tree_Store,
     Post   => (if In_Tree'Result then
                  not Is_Empty and Is_A_Valid_Tree_Node (N));

   function Key_Is_Present (K : Key_Type) return Boolean with
     Global => Tree_Store;
   function Node_Is_Present  (N : Tree_Node) return Boolean with
     Global => Tree_Store;
   function Is_Empty_Tree (T : Tree_Type) return Boolean with Ghost;
   function Persists (T_Pre, T_Post : Tree_Type) return Boolean with
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
     Pre    => In_Tree (N) and not Is_Empty_Tree (TS),
     Post   => not Is_Empty_Tree (TS) and
               Persists (TS'Old, TS) and Level (N) = Node_Level;
   procedure Set_Left  (N : Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N) and not Is_Empty_Tree (TS),
     Post   => not Is_Empty_Tree (TS) and Persists (TS'Old, TS) and
                  (if In_Tree (Branch)'Old then In_Tree (Branch));
   procedure Set_Right (N : Tree_Node; Branch : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N) and not Is_Empty_Tree (TS),
     Post   => not Is_Empty_Tree (TS) and Persists (TS'Old, TS) and
                  (if In_Tree (Branch)'Old then In_Tree (Branch));
   procedure Set_Key (N : Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N) and not Is_Empty_Tree (TS),
     Post   => not Is_Empty_Tree (TS) and Persists (TS'Old, TS) and
               Key_Is_Present (The_Key);
   procedure Set_Value (N : Tree_Node; Node_Value : Value_Type) with
     Global => (In_Out => Tree_Store),
     Pre    => In_Tree (N) and not Is_Empty_Tree (TS),
     Post   => not Is_Empty_Tree (TS) and Persists (TS'Old, TS);
   procedure Add_Node  (N : out Tree_Node; The_Key : Key_Type) with
     Global => (In_Out => Tree_Store),
     Post   => not Is_Empty_Tree (TS) and
               Persists (TS'Old, TS) and In_Tree (N) and
                  Key_Is_Present (The_Key);

   procedure Clear (N : Tree_Node) with
     Global => (In_Out => Tree_Store),
     Post   => not In_Tree (N);

private
--   type Tree_Node is range 0 .. Natural'Last - 1;
--   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
--   Empty_Node : constant Tree_Node := 0;

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Natural;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => 0,
      Left  => Empty_Node,
      Right => Empty_Node) with Ghost;

   type Tree_Type_Model is array (Valid_Tree_Node) of Actual_Node with Ghost;
   type Tree_Type is record
      Is_Empty : Boolean;
      Tree     : Tree_Type_Model;
   end record;

end SPARK_2014.Multi_Atree.Tree_Abs.Tree;
