----------------------------  SPARK_2014.Multi_Atree  ------------------------
--  This package facilitates storing multiple Atress an a single tree       --
--  structure.  The single tree structure which contains the mutiple        --
--  Atrees is denoted by the abstract state Tree_Container.                 --
--  The package is a specialisation of the SPARK_2014.Atrees package which  --
--  in turn uses a specialised SPARK_2014.Trees package, SPARK_2014_Tree.   --
------------------------------------------------------------------------------
with SPARK_2014.Bounded_Stacks;
package SPARK_2014.Multi_Atree with
  SPARK_Mode,
  Abstract_State => Tree_Container,
  Initializes => Tree_Container
is
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

   function Populated (ATree : A_Tree) return Boolean;

   procedure New_A_Tree (ATree : out A_Tree) with
     Global => (In_Out => Tree_Container),
     Post => Empty_Tree (ATree);

   function Count (ATree : A_Tree) return Natural;

   procedure Insert (ATree      : in out A_Tree;
                     Key        : Key_Type;
                     Inserted   : out Boolean) with
     Global => (In_Out => Tree_Container),
     Pre  => Count (ATree) < Natural'Last,
     Post => (if not Populated (ATree'Old) then
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
     Global => (In_Out => Tree_Container),
     Pre  => Count (ATree) < Natural'Last,
     Post => (if not Populated (ATree'Old) then
                Count (ATree) = 1
                  elsif Inserted then
                     Count (ATree) = Count (ATree'Old) + 1
                   else
                     Count (ATree) = Count (ATree'Old));

   procedure Clear_A_Tree (ATree       : in out A_Tree) with
     Global => (In_Out => Tree_Container),
     Pre => not Empty_Tree (ATree),
     Post => Empty_Tree (ATree);

   function Is_Equal (ATree_1, ATree_2 : A_Tree) return Boolean with
     Global => Tree_Container,
     Pre => Populated (ATree_1) and Populated (ATree_2);

   function Is_Present (ATree : A_Tree; Key : Key_Type) return Boolean with
     Global => Tree_Container,
     Pre  => Populated (ATree);

   function Tree_Depth (ATree : A_Tree) return Natural with
     Pre => Populated (ATree);

   type Enumerator is limited private;

   function New_Enumerator (ATree : A_Tree) return Enumerator with
     Pre => Populated (ATree);

   procedure Next_Node (E : in out Enumerator; Node : out Tree_Node) with
     Global => Tree_Container ;

private
   type Tree_Node is range 0 .. Natural'Last - 1;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;

   package Bounded_Stacks is new
     SPARK_2014.Bounded_Stacks (Tree_Node, Stack_Size);

   type Key_Type is new Natural;
   type Value_Type is new Integer;
   Null_Value : constant Value_Type := 0;

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stacks.Stack;
      end record;

   type A_Tree is
      record
         Root      : Tree_Node;
         Count     : Natural;
      end record;

   type Direction is (Left, Right);

end SPARK_2014.Multi_Atree;
