--------------------------  SPARK_2014.Tree_Abs  -----------------------------
--  A thin interface which provides a stateless abstraction to the          --
--  SPARK_2014-Tree package.                                       --
--  Any package using this abstraction should make it a private child of    --
--  the package and ensure the abstracion is sound. Only one tree can exist.--
--  All references to the underlying state have been removed and the        --
--  Persists has been removed.  The state has been abstracted into the      --
--  A_Tree objects.                                                      --
------------------------------------------------------------------------------
package SPARK_2014.Multi_Atree.Tree_Abs with
  SPARK_Mode
is
   --  type Tree_Node is private;
   --  Empty_Node : constant Tree_Node;
   --  function Is_A_Node (N : Tree_Node) return Boolean;
   --
   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean;

   --  subtype Key_Type is Natural;
   --  subtype Valid_Key_Type is Key_Type range
   --    Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   --  subtype Value_Type is Integer;
   --
   --  Null_Key : constant Key_Type := Key_Type'First;
   --  Null_Value : constant Value_Type := 0;
   --
   function Is_Empty (N : Tree_Node) return Boolean is (N = Empty_Node) with
     Inline;

   --  function Key_Of_Node (N : Tree_Node; N : Tree_Node) return Key_Type with
   --    Pre => In_A_Tree (N, T);

   function In_Tree  (N : Tree_Node) return Boolean with
     Post   => (if In_Tree'Result then
                  not Is_Empty (N) and Is_A_Valid_Tree_Node (N));
   --
   --  function Key_Is_Present (K : Key_Type; T : Tree_Node) return Boolean;
   --  function Node_Is_Present  (N : Tree_Node) return Boolean;
--   function Persists (T_Pre, T_Post : Tree_Node) return Boolean;

   function Level (N : Tree_Node) return Node_Count with
     Pre    => In_Tree (N);
   function Left  (N : Tree_Node) return Tree_Node with
     Pre    => In_Tree (N),
     Post   => (if not Is_Empty (Left'Result) then
                  In_Tree (Left'Result));
   function Right (N : Tree_Node) return Tree_Node with
     Pre    => In_Tree (N),
     Post   => (if not Is_Empty (Right'Result) then
          In_Tree (Right'Result));
   function Key (N : Tree_Node) return Key_Type with
     Pre    => In_Tree (N);
   function Value (N : Tree_Node) return Value_Type with
     Pre    => In_Tree (N);

   procedure Set_Level (N : in out Tree_Node; Node_Level : Node_Count) with
     Pre    => In_Tree (N),
     Post   => In_Tree (N) and Level (N) = Node_Level;
   procedure Set_Left  (N : in out Tree_Node; Branch : Tree_Node) with
     Pre    => In_Tree (N),
     Post   => In_Tree (N) and Left (N) = Branch;
   procedure Set_Right (N : in out Tree_Node; Branch : Tree_Node) with
     Pre    => In_Tree (N),
     Post   => In_Tree (N) and Right (N) = Branch;
   procedure Set_Key (N : in out Tree_Node; The_Key : Key_Type) with
     Pre    => In_Tree (N),
     Post   => In_Tree (N) and Key (N) = The_Key;
   procedure Set_Value (N : in out Tree_Node; Node_Value : Value_Type) with
     Pre    => In_Tree (N),
     Post   => In_Tree (N) and Value (N) = Node_Value;
   procedure Add_Node  (N : out Tree_Node; The_Key : Key_Type) with
     Post   => In_Tree (N) and Key (N) = The_Key;

   procedure Clear (N : in out Tree_Node) with
     Post   => Is_Empty (N);

private
   --  type Tree_Node is range 0 .. Natural'Last - 1;
   --  subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   --  Empty_Node : constant Tree_Node := 0;

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

end SPARK_2014.Multi_Atree.Tree_Abs;
