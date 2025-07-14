--------------------------  SPARK_2014.Tree_Abs  -----------------------------
--  This package is a stateless abstraction providing basic operations on   --
--  an underlying tree.  The tree is represented by its collection of       --
--  Tree_Nodes, which are also the embodiment of the state hidden within    --
--  the package body. The body of this package must be excluded from SPARK  --
--  analysis because of the hidden state.                                   --                                                          --
--  The collection of all Tree_Nodes represents a single tree.  There is    --
--  no type to enable multiple tree decarations.  If multiple trees are     --
--  required then, either multiple instatnces of this package are required  --
--  or multiple sub-trees have to be accommodated within the single tree    --
--  structure this package provides.                                        --
------------------------------------------------------------------------------
package SPARK_2014.Multi_Atree.Tree_Abs with
  SPARK_Mode
is
   --  The following declarations should be declared in the parent package.
   --  type Tree_Node is private;
   --  Empty_Node : constant Tree_Node;
   --  function Is_A_Node (N : Tree_Node) return Boolean;
   --
   --  subtype Key_Type is Natural;
   --  subtype Valid_Key_Type is Key_Type range
   --    Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   --  subtype Value_Type is Integer;
   --
   --  Null_Key : constant Key_Type := Key_Type'First;
   --  Null_Value : constant Value_Type := 0;

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean with Inline;

   function Is_Empty (N : Tree_Node) return Boolean is (N = Empty_Node) with
     Inline;

   function In_Tree  (N : Tree_Node) return Boolean with
     Post   => (if In_Tree'Result then
                  not Is_Empty (N) and Is_A_Valid_Tree_Node (N));

   function Level (N : Valid_Tree_Node) return Node_Count;
   function Left  (N : Valid_Tree_Node) return Tree_Node with
     Post   => (if not Is_Empty (Left'Result) then
                  In_Tree (Left'Result));
   function Right (N : Valid_Tree_Node) return Tree_Node with
     Post   => (if not Is_Empty (Right'Result) then
          In_Tree (Right'Result));
   function Key (N : Valid_Tree_Node) return Key_Type;
   function Value (N : Valid_Tree_Node) return Value_Type;

   -- In the following procedure declarations the Tree_Node parameter, N,
   -- has been designated as mode in out, even though N is not modified
   -- by calling them.  This is how the underlying state hidden in the
   -- package body is represented in this abstraction.
   procedure Set_Level (N : in out Valid_Tree_Node; Node_Level : Node_Count)
   with
     Post   => Level (N) = Node_Level;
   procedure Set_Left  (N : in out Valid_Tree_Node; Branch : Tree_Node) with
     Post   => Left (N) = Branch;
   procedure Set_Right (N : in out Valid_Tree_Node; Branch : Tree_Node) with
     Post   => Right (N) = Branch;
   procedure Set_Key (N : in out Valid_Tree_Node; The_Key : Key_Type) with
     Post   => Key (N) = The_Key;
   procedure Set_Value (N : in out Valid_Tree_Node; Node_Value : Value_Type) with
     Post   => Value (N) = Node_Value;
   procedure Add_Node  (N : out Valid_Tree_Node; The_Key : Key_Type) with
     Post   => Key (N) = The_Key;

   --  N may be modified by the following procedures.
   procedure Clear_Tree_Below_Node (N : in out Tree_Node) with
     Pre  => Is_A_Valid_Tree_Node (N),
     Post => Is_Empty (N);

   procedure Free;

private
   --  The following declarations should be placed in the parent package
   --  type Tree_Node is range 0 .. Natural'Last - 1;
   --  subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   --  Empty_Node : constant Tree_Node := 0;

   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Node_Count;
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
