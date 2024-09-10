----------------------------  SPARK_2014.Multi_Atree  ------------------------
--  This package facilitates storing multiple A_Trees within an a single    --
--  single tree structure.                                                  --
--  The single tree structure which contains the mutiple A_Trees            --
--  is provided by the tree abstraction child package                       --
--  SPARK_2014.Multi_Atree.Tree_Abs which can only support 1 tree but       --
--  using this package multiple A_Trees can be stored within it.            --
------------------------------------------------------------------------------
with SPARK_2014.Bounded_Stacks;
package SPARK_2014.Multi_Atree with
  SPARK_Mode,
  Abstract_State => Status,
  Initializes    => Status
--  The following Initial_Condition does not work with 2021 version of
--  SPARK_2014 as it does not seem to realise that Is_Building has an
--  Input global, Status, and gives an error that the Status must be mentioned
--  in theInitializes aspect of Multi_Atree
  --  Initial_Condition => not Is_Building
is
   function Is_Building return Boolean with
     Global => Status;

   -- Notional maximum nodes in a tree.
   type Node_Count is range 0 .. Natural'Last - 1;
   type Key_Type is private;
   type Value_Type is private;
   Null_Value : constant Value_Type;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   Stack_Size : constant Positive := 32;
   type Tree_Node is private;
   Empty_Node : constant Tree_Node;
   type Valid_Tree_Node is private;

   type A_Tree is private;

   function Empty_Tree (ATree : A_Tree) return Boolean;

   function In_A_Tree (N : Tree_Node; Tree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree) return Boolean with
     Post => (if Populated'Result then (Count (ATree) > 0));

   function Building (ATree : A_Tree) return Boolean with
     Global => Status,
     Post   => (if Building'Result then Is_Building);

   procedure New_A_Tree (ATree : out A_Tree) with
     Global => (In_Out => Status),
     Pre    => not Is_Building,
     Post => Empty_Tree (ATree) and Is_Building and Building (ATree) and
             Count (ATree) = 0;

   function Count (ATree : A_Tree) return Node_Count;

   procedure Insert (ATree      : in out A_Tree;
                     Key        : Key_Type;
                     Inserted   : out Boolean) with
     Pre    => Building (ATree) and Count (ATree) < Node_Count'Last,
     Post   => Building (ATree) and
              (if not Populated (ATree'Old) then
                Count (ATree) = 1
                  elsif Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Insert_With_Value (ATree         : in out A_Tree;
                                Key           : Key_Type;
                                Value         : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type) with
     Pre  => Building (ATree) and Count (ATree) < Node_Count'Last,
     Post => Building (ATree) and
             (if not Populated (ATree'Old) then
                Count (ATree) = 1
                  elsif Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Clear_A_Tree (ATree       : in out A_Tree) with
     Global => (In_Out => Status),
     Pre => Building (ATree),
     Post => not Building (ATree) and not Is_Building and Empty_Tree (ATree);

   function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean with
     Pre => Populated (ATree_1) and Populated (ATree_2);

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean with
     Pre  => Populated (ATree);

   function Tree_Depth (ATree : A_Tree) return Node_Count with
     Pre => Populated (ATree);

   type Enumerator is limited private;

   function New_Enumerator (ATree : A_Tree) return Enumerator with
     Pre => Populated (ATree);

   procedure Next_Node (E : in out Enumerator; The_Node : out Tree_Node);

private
   type Tree_Node is new Node_Count;
   type Valid_Tree_Node is new Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := 0;

   package Bounded_Stacks is new
     SPARK_2014.Bounded_Stacks (Valid_Tree_Node, Stack_Size);

   type Key_Type is new Natural;
   subtype Valid_Key_Type is Key_Type range
     Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   type Value_Type is new Integer;
   Null_Key : constant Key_Type := Key_Type'First;
   Null_Value : constant Value_Type := 0;

  type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stacks.Stack;
      end record;

   type Statuses is (Unassigned, Constructing, Free);
   subtype A_Tree_Status is Statuses range Unassigned .. Constructing;
   subtype Pack_Status   is Statuses range Constructing .. Free;
   type A_Tree is
      record
         Root        : Tree_Node;
         Target_Node : Tree_Node;  -- Node of interest for applied operation
         Count       : Node_Count;
         State       : A_Tree_Status;
      end record;

   type Direction is (Left, Right);

end SPARK_2014.Multi_Atree;
