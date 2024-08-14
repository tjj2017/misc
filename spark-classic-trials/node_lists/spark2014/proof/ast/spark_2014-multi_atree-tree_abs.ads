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
   --  function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean;

   --  subtype Key_Type is Natural;
   --  subtype Valid_Key_Type is Key_Type range
   --    Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   --  subtype Value_Type is Integer;
   --
   --  Null_Key : constant Key_Type := Key_Type'First;
   --  Null_Value : constant Value_Type := 0;
   --
   function Is_Empty (N : Tree_Node) return Boolean with Inline;

   function Key_Of_Node (N : Tree_Node; T : A_Tree) return Key_Type with
     Pre => In_A_Tree (N, T);

   --  function In_Tree  (N : Tree_Node) return Boolean with
   --    Post   => (if In_Tree'Result then
   --                 not Is_Empty (N) and Is_A_Valid_Tree_Node (N));
   --
   --  function Key_Is_Present (K : Key_Type; T : Tree_Node) return Boolean;
   --  function Node_Is_Present  (N : Tree_Node) return Boolean;
--   function Persists (T_Pre, T_Post : Tree_Node) return Boolean;

   function Level (T : A_Tree) return Natural with
     Pre    => Target_Node_In_Tree (T);
   function Left  (T : A_Tree) return Tree_Node with
     Pre    => Target_Node_In_Tree (T),
     Post   => (if not Is_Empty (Left'Result) then
                  In_A_Tree (Left'Result, T));
   function Right (T : A_Tree) return Tree_Node with
     Pre    => Target_Node_In_Tree (T),
     Post   => (if not Is_Empty (Right'Result) then
          In_A_Tree (Right'Result, T));
   function Key (T : A_Tree) return Key_Type with
     Pre    => Target_Node_In_Tree (T);
   function Value (T : A_Tree) return Value_Type with
     Pre    => Target_Node_In_Tree (T);

   procedure Set_Level (T : in out A_Tree; Node_Level : Natural) with
     Pre    => Target_Node_In_Tree (T),
     Post   => Target_Node_In_Tree (T) and Level (T) = Node_Level;
   procedure Set_Left  (T : in out A_Tree; Branch : Tree_Node) with
     Pre    => Target_Node_In_Tree (T),
     Post   => Target_Node_In_Tree (T) and Left (T) = Branch;
   procedure Set_Right (T : in out A_Tree; Branch : Tree_Node) with
     Pre    => Target_Node_In_Tree (T),
     Post   => Target_Node_In_Tree (T) and Right (T) = Branch;
   procedure Set_Key (T : in out A_Tree; The_Key : Key_Type) with
     Pre    => Target_Node_In_Tree (T),
     Post   => Target_Node_In_Tree (T) and Key (T) = The_Key;
   procedure Set_Value (T : in out A_Tree; Node_Value : Value_Type) with
     Pre    => Target_Node_In_Tree (T),
     Post   => Target_Node_In_Tree (T) and Value (T) = Node_Value;
   procedure Add_Node  (T : in out A_Tree; The_Key : Key_Type) with
     Post   => Target_Node_In_Tree (T) and Key (T) = The_Key;

   procedure Clear (T : in out A_Tree) with
     Post   => Empty_Tree (T);

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
