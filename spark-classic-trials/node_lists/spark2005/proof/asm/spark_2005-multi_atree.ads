----------------------------  SPARK_2014.Multi_Atree  ------------------------
--  This package facilitates storing multiple A_Trees within an a single    --
--  single tree structure.                                                  --
--  The single tree structure which contains the mutiple A_Trees            --
--  is provided by the generic package SPARK_2014.Tree_Abstraction          --
--  which can only support 1 tree but using this package multiple A_Trees   --
--  can be stored within it.                                                --
------------------------------------------------------------------------------
with SPARK_CLassic.Bounded_Stacks;
package SPARK_Classic.Multi_Atree
--# own Status;
is
   function Is_Building return Boolean;
   --# global Status;

   --  Call A_Tree_Init to initialise the A_Tree subsystem prior to use.
   --  Deletes all A_Trees - only use as an initialisation.
   procedure A_Tree_Init;
   --# global out Status;
   --# post not Is_Building;

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

   type A_Tree is private;

   function Empty_Tree (ATree : A_Tree) return Boolean;

   function In_A_Tree (N : Tree_Node; Tree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree) return Boolean with
     Post => (if Populated'Result then Count (ATree) /= 0);

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
     Post   => Building (ATree) and Populated (ATree) and
              (if not Populated (ATree'Old) and Inserted then
                Count (ATree) = 1
                  elsif Populated (ATree'Old) and Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Insert_With_Value (ATree         : in out A_Tree;
                                Key           : Key_Type;
                                Value         : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type) with
     Pre  => Building (ATree) and Count (ATree) < Node_Count'Last,
     Post => Building (ATree) and Populated (ATree) and
             (if not Populated (ATree'Old) and Inserted then
                Count (ATree) = 1
                  elsif Populated (ATree'Old) and Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   --  This procedure Clear_A_Tree may be used to clear the A_Tree
   --  currently under construction.  An A_Tree whose construction has been
   --  completed cannot be cleared.  It is permanent until the tree structure
   --  is re-initialised with Init.  Init deletes all A_Trees.
   procedure Clear_A_Tree (ATree : in out A_Tree) with
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
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := Tree_Node'First;

   package Bounded_Stacks is new
     SPARK_2014.Bounded_Stacks (Valid_Tree_Node, Stack_Size);

   type Key_Type is new Natural;
   type Value_Type is new Integer;
   subtype Valid_Key_Type is Key_Type range
     Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   Null_Key : constant Key_Type := Key_Type'First;
   Null_Value : constant Value_Type := 0;

   package Tree_Abs is new SPARK_2014.Tree_Abstraction
     (Tree_Node  => Tree_Node,
      Level_Type => Node_Count,
      Key_Type   => Key_Type,
      Value_Type => Value_Type,
      Null_Key   => 0,
      Null_Value => 0);

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

end SPARK_Classic.Multi_Atree;
