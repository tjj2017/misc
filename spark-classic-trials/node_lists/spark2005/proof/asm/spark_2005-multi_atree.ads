----------------------------  SPARK_2014.Multi_Atree  ------------------------
--  This package facilitates storing multiple A_Trees within an a single    --
--  single tree structure.                                                  --
--  The single tree structure which contains the mutiple A_Trees            --
--  is provided by the generic package SPARK_2014.Tree_Abstraction          --
--  which can only support 1 tree but using this package multiple A_Trees   --
--  can be stored within it.                                                --
------------------------------------------------------------------------------
--# inherit SPARK_2005;
package SPARK_2005.Multi_Atree
--# own Status;
is
   function Is_Building return Boolean;
   --# global Status;

   --  Call A_Tree_Init to initialise the A_Tree subsystem prior to use.
   --  Deletes all A_Trees - only use as an initialisation.
   procedure A_Tree_Init;
   --# global out Status;
   --# post not Is_Building (Status);

   -- Notional maximum nodes in a tree.
   type Node_Count is range 0 .. Natural'Last - 1;
   type Key_Type is private;
   type Value_Type is private;
   Null_Value : constant Value_Type;

   type Tree_Node is private;

   type A_Tree is private;

   function Empty_Tree (ATree : A_Tree) return Boolean;
   function Count (ATree : A_Tree) return Node_Count;

   function In_A_Tree (N : Tree_Node; Tree : A_Tree) return Boolean;

   function Populated (ATree : A_Tree) return Boolean;
   --# return Populated (ATree) -> Count (ATree) /= 0;

   function Building (ATree : A_Tree) return Boolean;
   --# global Status;
   --  --# return B => B -> Is_Building (Status);

    --# function Node_Key (Node : Tree_Node; ATree : A_Tree) return Key_Type;
   --# pre In_A_Tree (Node, ATree);

   procedure New_A_Tree (ATree : out A_Tree);
   --# global in out Status;
   --# pre not Is_Building (Status);
   --# post Empty_Tree (ATree) and Is_Building (Status) and
   --#      Building (ATree, Status) and
   --#      Count (ATree) = 0;

   procedure Insert (ATree      : in out A_Tree;
                     Key        : Key_Type;
                     Inserted   : out Boolean;
                     Key_Node   : out Tree_Node);
   --# global in Status;
   --# pre  Building (ATree, Status) and Count (ATree) < Node_Count'Last;
   --# post Building (ATree, Status) and Populated (ATree) and
   --#         (not Populated (ATree~)) and (Inserted ->
   --#           (Count (ATree) = 1)) and
   --#         Populated (ATree~) and (Inserted ->
   --#           (Count (ATree) = Count (ATree~) + 1)) and
   --#         (not Inserted -> (Count (ATree) = Count (ATree~))) and
   --#        In_A_Tree (Key_Node, ATree) and Node_Key (Key_Node, ATree) = Key;

   --  This procedure Clear_A_Tree may be used to clear the A_Tree
   --  currently under construction.  An A_Tree whose construction has been
   --  completed cannot be cleared.  It is permanent until the tree structure
   --  is re-initialised with Init.  Init deletes all A_Trees.
   procedure Clear_A_Tree (ATree : in out A_Tree);
   --# global in out Status;
   --# pre  Building (ATree, Status);
   --# post not Building (ATree, Status) and not Is_Building (Status)
   --#      and Empty_Tree (ATree);

   function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean;
   --# pre Populated (ATree_1) and Populated (ATree_2);

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean;
   --# pre  Populated (ATree);

   function Tree_Depth (ATree : A_Tree) return Node_Count;
   --# pre Populated (ATree);

   type Enumerator is limited private;

   procedure New_Enumerator (ATree : A_Tree; New_Enum : out Enumerator);
   --# pre Populated (ATree);

   procedure Next_Node (E : in out Enumerator; The_Node : out Tree_Node);

private
   type Tree_Node is range Node_Count'First .. Node_Count'Last;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := Tree_Node'First;

   type Key_Type is range Natural'First .. Natural'Last;
   type Value_Type is new Integer;
   subtype Valid_Key_Type is Key_Type range
     Key_Type'Succ (Key_Type'First) .. Key_Type'Last;
   Null_Key : constant Key_Type := Key_Type'First;
   Null_Value : constant Value_Type := 0;

   --  To traverse the tree a stack is required. The Stack_Size must be large
   --  enough to traverse the tree without overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   --  A size of 32 is sufficient for more than the number of possible nodes.
    Stack_Size : constant Node_Count := 32;

   --  private to child Stack_Ops -----------------------------------
   --  Only the subprograms declared in the visible part of        --
   --  should directly manipulate these declarations.              --
   subtype Stack_Count is Node_Count range 0 .. Stack_Size;        --
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last; --
   type Stack_Contents is array (Stack_Index) of Valid_Tree_Node;  --
                                                                   --
   type Stack is  -- private                                       --
      record                                                       --
         Count    : Stack_Count;                                    --
         Contents : Stack_Contents;                                --
      end record;                                                  --
   ------------------------------------------------------------------

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Stack;
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

end SPARK_2005.Multi_Atree;
