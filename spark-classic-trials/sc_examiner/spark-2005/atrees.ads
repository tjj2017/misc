----------------------------------  Atrees  ----------------------------------
--  This package provides an implementaion of Anderson balanced trees using --
--  a Basic_Tree.Tree object to store the nodes of the tree.                --
--  Anderson Trees are accessed via a type A_Tree object A new Anderson tree --
--  is established by calling the New_Atree procedure to associate the      --
--  A_Tree object with the Basic_Tree.Tree object.                           --
--  All interactions with the Anderson tree are accomplished using the      --
--  subprograms in this psckage declaration using the Atree and Basic_Tree  --
--  objects as parameters.
--  Procedure Init should be called to initialise the underlying tree used  --
--  to store the A_Tree objects.                                            --
------------------------------------------------------------------------------

with Specific_Tree_Types;
with Basic_Tree;
with Bounded_Stacks;
package Atrees is

   type A_Tree is private;

   subtype Host_Tree is Basic_Tree.Tree;
   subtype Key_Type is Specific_Tree_Types,Key_Type;
   subtype Value_Type is Specific_Tree_Types.Value_Type;
   Null_Key : constant Key_Type := Specific_Tree_Types.Null_Key;
   Null_Value : Value_Type;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   --  Stack_Size : Positive

   type A_Tree is private;

   --# function Empty_Atree (ATree : A_Tree; Host : Host_Tree) return Boolean;
   --# function Key_In_Atree (Atree : A_Tree; Host : Host_Tree) return Boolean;
   --# function Value_From_Key (Atree : A_Tree;
   --#                          Host : Host_Tree;
   --#                          Key : Key_Type) return Value_Type;

   function Count (ATree : A_Tree; Host : Host_Tree) return Natural;

   function Tree_Depth (ATree : A_Tree; Host : Host_Tree) return Natural;

   function Is_Present (ATree : A_Tree; Host : Host_Tree; Key : Key_Type)
                        return Boolean;
   --# pre not Empty_Atree (Atree, Host);
   --# return Key_In_Atree (Atree, Host, Key);

   function Value (ATree : A_Tree; Host : Host_Tree; Key : Key_Type)
                   return Value_Type;
   --# pre not Empty_Atree (Atree, Host);
   --# return Value_From_Key (Atree, Host, Key);

   function Equal_Keys (Atree_1, Atree_2 : A_Tree,
                        Host_1, Host_2 : Host_Tree) return Boolean;
   --# not Empty_Atree (Atree_1, Host_1) and not Empty_Atree (Atree_2, Host_2);
   --# return => (Count (Atree_1, Host_1) = Count (Atree_2, Host_2) and then
   --#           (for all I in Natural range 0 .. Count =>
   --#               (Key_At_Index (First_Index (Atree_1) + Node_Index (I)) =
   --#                Key_At_Index (First_Index (Atree_2) + Node_Index (I))));

   with Pre => Populated (ATree_1) and Populated (ATree_2);

   function Equal_Keys_And_Values (ATree_1, ATree_2 : A_Tree) return Boolean
   with Pre => Populated (ATree_1) and Populated (ATree_2);

   procedure New_A_Tree (Tree : out A_Tree)
     with Post => Empty_Tree (Tree);

   procedure Insert (Tree      : in out A_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean)
     with Pre  => Count (Tree) < Natural'Last,
     Post => Is_Present (Tree, Key) and
             (if not Populated (Tree'Old) then
                Count (Tree) = 1
              elsif Inserted then
                Count (Tree) = Count (Tree'Old) + 1
               else
                Count (Tree) = Count (Tree'Old));

   procedure Insert_With_Value (Tree          : in out A_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type)
     with Pre  => Count (Tree) < Natural'Last,
     Post => Is_Present (Tree, Key) and
             (if Inserted then Value (Tree, Key) = Insert_Value) and
             (if not Populated (Tree'Old) then
                Count (Tree) = 1
                  elsif Inserted then
                     Count (Tree) = Count (Tree'Old) + 1
                   else
                  Count (Tree) = Count (Tree'Old));

   --  *** The following subprograms should be used with care. ***
   --  *** They operate on the underlying tree structure and   ***
   --  *** are not A_Tree object aware.                        ***

   function Last_Underlying_Tree_Node (Dummy : Atree_Node) return Atree_Node;
   --  Returns the last used node in the underlying tree. Each successful
   --  insertion (subprogram Insert parameter Inserted = True) creates a
   --  new node in the underlying tree. The Last_Underlying_Tree_Node will
   --  be the one created from the last successful Insert.

   procedure Clear_Underlying_Tree_From_Node (Node : in out Atree_Node);
   --  Removes all nodes below the given Node from the underlying tree.
   --  This will invalidate all A_Tree objects

   --  *************************************************************

   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   function New_Enumerator (ATree : A_Tree) return Enumerator
     with Pre => Populated (ATree);

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type);

   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type);

private
   Empty_Node : constant Atree_Node := Atree_Node'First;

   package Bounded_Stack is new
     Bounded_Stacks (Atree_Node, Stack_Size);

   type Enumerator is
      record
         ATree   : A_Tree;
         --  A stack to record visited nodes when enumerating.
         Visited : Bounded_Stack.Stack;
      end record;

   type A_Tree is
      record
         Root      : Atree_Node;
         Count     : Natural;
         Toggle    : Boolean;
      end record;

   type Direction is (Left, Right);

end Atrees;
