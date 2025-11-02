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

   --  function Count returns the number of Keys in the Atree.
   function Count (Atree : A_Tree; Host : Host_Tree) return Natural;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.
   --  Stack_Size : Positive

   --  The following type declaration and proof functions allow specifications
   --  in terms of the underlying host tree TYPE Node_Index which is a scalar
   --  which can be used in quantifiers,

   --  Logically,each Key in he tree has an index.  Keys are indexed
   --  consecutively such that for all keys in the Atree the value of
   --  Indexed_Key (I + 1) > Indexed_Key (I).  The keysareordered by index
   --  and there are no duplicate keys.
   --# function Indexed_Key (Index : Positive;
   --#                       Atree : A_Tree;
   --#                       Host : Host_Tree) return Key_Type;


   --  The following proof functions are used in specifying the executable
   --  subprograms declared in the declaration of this package.

   --# function Populated (Atree : A_Tree, Host : Host_Tree) return Boolean;
   --# return Count (Atree) > 0;

   function Tree_Depth (Atree : A_Tree; Host : Host_Tree) return Natural;

   function Is_Present (Atree : A_Tree; Host : Host_Tree; Key : Key_Type)
                        return Boolean;
   --# pre Populated (Atree, Host);
   --# return for some I in Positive range 1 .. Count (Atree) =>
   --#           Index_Key (I, Atree, Host) = Key;

   function Value (ATree : A_Tree; Host : Host_Tree; Key : Key_Type)
                   return Value_Type;
   --# pre Populated (Atree, Host);
   --# return V => for some I in Positive range 1 .. Count(Find_Key (Atree, Host, Key) /= Null_Index) ->
   --#           (Basic_Tree.Value (Find_Key (Atree, Host, Key)) = Value);

   function Equal_Keys (Atree_1, Atree_2 : A_Tree,
                        Host_1, Host_2 : Host_Tree) return Boolean;
   --# pre Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2);
   --# return  (Count (Atree_1) = Count (Atree_2)) and then
   --#         ((Next_Key_Index (Atree_1, Host_1, Base_Index (Atree_1))
   --#                                                     /= Null_Index
   --#           and
   --#           (Next_Key_Index (Atree_1, Host_1, Base_Index (Atree_1)) =
   --#           Next_Key_Index (Atree_2, Host_2, Base_Index (Atree_2))

     (Count (Atree_1, Host_1) = Count (Atree_2, Host_2) and then
   --#           (for all I in Natural range 0 .. Count =>
   --#               (Basic_Tree.Key (First_Index (Atree_1) + Node_Index (I)) =
   --#                Basic_Tree.Key (First_Index (Atree_2) + Node_Index (I))));

   function Equal_Keys_And_Values (Atree_1, Atree_2 : A_Tree;
                                   Host_1, Host_2 : Host_Tree) return Boolean;
   --# pre Populated (Atree_1, Host_1) and Populated (Atree_2, Host_2);
   --# return => (Count (Atree_1, Host_1) = Count (Atree_2, Host_2) and then
   --#           (for all I in Natural range 0 .. Count =>
   --#               (Basic_Tree.Key (First_Index (Atree_1) + Node_Index (I)) =
   --#                Basic_Tree.Key (First_Index (Atree_2) + Node_Index (I))
   --#                and
   --#                Basic_Tree.Value (First_Index (Atree_1) + Node_Index (I) =
   --#                Basic_Tree.Value (First_Index


   --# return Current_Index (Atree, Host) /= Null_Index and then
   --#        (Basic_Tree.Key (Current_Index (Atree, Host)) = Key and
   --#        Basic_Tree.Key (Current_Index (Atree, Host)) = Value);


   procedure New_A_Tree (Atree : out A_Tree; Host : in out Host_Tree);
   --# post Empty_Atree (Atree) and not Basic_Tree.Empty_Tree (Host) and
   --#      Last_Index = Null_Index;

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
