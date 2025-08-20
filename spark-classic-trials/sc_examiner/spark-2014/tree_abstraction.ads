----------------------------  Tree_Abstraction  ------------------------------
--  This package is a stateless abstraction providing basic operations on   --
--  an underlying tree.  The tree is represented by its collection of       --
--  Tree_Nodes, which are also the embodiment of the state hidden within    --
--  the package body. The body of this package must be excluded from SPARK  --
--  analysis because of the hidden state.  The stateless view of the        --
--  package overcomes limitations of the 2021 SPARK-2012 examiner handling  --
--  state_abstraction in generic packages.                                  --
--  The collection of all Tree_Nodes represents a single tree.  There is    --
--  no type to enable multiple tree decarations.  If multiple trees are     --
--  required then, either multiple instatnces of this package are required  --
--  or multiple sub-trees have to be accommodated within the single tree    --
--  structure this package provides.                                        --
--  The type Tree_Node, representing the nodes of the tree,                 --
--  must have the range 0 .. Maximum_Number_Of_Nodes in the tree.           --
--  The Maximum_Number_Of_Nodes_In_Tree < Tree_Node'Base'Last.              --
--  Tree_Node'First represents the Empty_Node.                              --
--  Each node of the Tree has 5 fields:                                     --
--  Left, Right : Tree_Node;   --  the children of the node                 --
--  Level       : Level_Type;  --  the level of the node in the Tree        --
--  Key         : Key_Type;    --  place for key of node                    --
--  Value       : Value_Type;  --  place for value of node                  --
--  Both Key and Value need to have a null value provided.                  --
--  The values of these fields are set and interrogated by the subprograms  --
--  declared below.                                                         --
--  Procedure Init should be called prior to using the tree structure.      --
------------------------------------------------------------------------------
generic
   type Tree_Node is range <>;

   type Level_Type is range <>;
   type Key_Type is private;
   type Value_Type is private;
   Null_Key : Key_Type;
   Null_Value : Value_Type;

package Tree_Abstraction with
  SPARK_Mode
is
   procedure Init;

   --  Empty_Node must equal Tree_Node'First
   Empty_Node : constant Tree_Node := Tree_Node'First;
   --  Valid_Tree_Node must exclude the Empty_Node, i.e.
   --  range Tree_Node'First + 1 .. Tree_Node'Last.
   subtype Valid_Tree_Node is Tree_Node range
     Tree_Node'First + 1 .. Tree_Node'Last;
   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean with Inline;

   function Is_Empty (N : Tree_Node) return Boolean is (N = Empty_Node) with
     Inline;

   function In_Tree  (N : Tree_Node) return Boolean with
     Post   => (if In_Tree'Result then
                  not Is_Empty (N) and Is_A_Valid_Tree_Node (N) and
                  N in Valid_Tree_Node),
     Inline;

   function Level (N : Valid_Tree_Node) return Level_Type with Inline;
   function Left  (N : Valid_Tree_Node) return Tree_Node with
     Post   => (if not Is_Empty (Left'Result) then
                  In_Tree (Left'Result)),
     Inline;
   function Right (N : Valid_Tree_Node) return Tree_Node with
     Post   => (if not Is_Empty (Right'Result) then
                  In_Tree (Right'Result)),
     Inline;
   function Key (N : Valid_Tree_Node) return Key_Type with Inline;
   function Value (N : Valid_Tree_Node) return Value_Type with Inline;

   -- In the following procedure declarations the Tree_Node parameter, N,
   -- has been designated as mode in out, even though N is not modified
   -- by calling them.  This is how the underlying state hidden in the
   -- package body is represented in this abstraction.
   procedure Set_Level (N : in out Valid_Tree_Node; Node_Level : Level_Type)
   with
       Post   => Level (N) = Node_Level and In_Tree (N),
       Inline;
   procedure Set_Left  (N : in out Valid_Tree_Node; Branch : Tree_Node) with
     Post   => Left (N) = Branch and In_Tree (N),
     Inline;
   procedure Set_Right (N : in out Valid_Tree_Node; Branch : Tree_Node) with
     Post   => Right (N) = Branch and In_Tree (N),
     Inline;
   procedure Set_Key (N : in out Valid_Tree_Node; The_Key : Key_Type) with
     Post   => Key (N) = The_Key and In_Tree (N),
     Inline;
   procedure Set_Value (N : in out Valid_Tree_Node; Node_Value : Value_Type) with
     Post   => Value (N) = Node_Value and In_Tree (N),
     Inline;
   procedure Add_Node  (N : out Valid_Tree_Node; The_Key : Key_Type) with
     Post   => Key (N) = The_Key and In_Tree (N),
     Inline;

   --  N may be modified by the following procedures.
   procedure Clear_Tree_Below_Node (N : in out Tree_Node) with
     Pre  => Is_A_Valid_Tree_Node (N),
     Post => Is_Empty (N);
   --  Removes all nodes from the Tree with Tree_Node values >= N.

private
   type Actual_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Level : Level_Type;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Key   => Null_Key,
      Value => Null_Value,
      Level => 0,
      Left  => Empty_Node,
      Right => Empty_Node);

end Tree_Abstraction;
