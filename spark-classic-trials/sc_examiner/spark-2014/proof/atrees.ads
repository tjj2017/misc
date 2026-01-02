----------------------------------  Atrees  -----------------------------------
--  This package provides an implementaion of Andersson balanced trees using --
--  a Basic_Tree.Tree object to store the nodes of the tree.                 --
--  Andersson Trees are accessed via a type A_Tree object A new Andersson    --
--  tree is established by calling the New_Atree procedure to associate the  --
--  A_Tree object with the Basic_Tree.Tree object.                           --
--  All interactions with the Andersson tree are accomplished using the      --
--  subprograms in this package declaration using the Atree and Basic_Tree   --
--  objects as parameters.                                                   --
--  The host Basic Tree object must be initialized by a call Init_Host_Tree. --
-------------------------------------------------------------------------------

with Basic_Tree;
with Bounded_Stacks;
generic
   type Key_Type is (<>);
   type Value_Type is private;

   Null_Key : in Key_Type;
   Null_Value : in Value_Type;

   Max_Nodes_In_Tree : in Positive;

   Stack_Size : in Positive;

package Atrees with
SPARK_Mode
is
   type Host_Tree is private;  --  Should be a limited type.

   procedure Init_Host_Tree (Host : out Host_Tree);

   type A_Tree is private;

   subtype Key_Count is Natural range 0 .. Max_Nodes_In_Tree;

   --  function Count returns the number of nodes in the Atree.
   function Count (Atree : A_Tree) return Key_Count with
   Inline;

   --                          Proof Functions                             --
   --------------------------------------------------------------------------
   --  The following proof functions are used in specifying the A_Tree
   --  subprograms.

   --  Proof function denoting the Host tree od the given Atree.
   function Host (Atree : A_Tree) return Host_Tree with Ghost;

   --  Logically,each Key in he tree has an index.  Keys are indexed
   --  consecutively such that for all keys in the Atree the value of
   --  Indexed_Key (I + 1) > Indexed_Key (I).  The keys are ordered by index
   --  and there are no duplicate keys.
   subtype Key_Index is Key_Count with
     Ghost;
   First_Index : constant Key_Index := Key_Index'Succ (Key_Index'First) with
     Ghost;
   subtype Valid_Key_Index is Key_Count range First_Index .. Key_Index'Last with
     Ghost;
   Null_Key_Index : constant Key_Index := Key_Index'First with Ghost;

   --  Given a Key_Index returns the corresponding Key.
   function Indexed_Key (Atree : A_Tree;
                         Index : Valid_Key_Index) return Key_Type with
     Pre => Count (Atree) > 0 and then Index <= Count (Atree),
     Post => Indexed_Key'Result /= Null_Key,
     Ghost;

   --  Defines the ordering of an A_Tree.
   function Ordered (Atree : A_Tree) return Boolean is
     (for all I in Valid_Key_Index range First_Index .. Count (Atree) - 1 =>
           Indexed_Key (Atree, I + 1) > Indexed_Key (Atree, I)) with
       Ghost;

   --  Indicates that an A_Tree is non empty.
   function Populated (Atree : A_Tree) return Boolean with
     Post => (if Populated'Result then Count (Atree) > 0),
     Ghost;

   --  Given a Key_Index returns the Value associated with the A_Tree node
   --  with the Key indexed by the Key_Index.
   function Value_At_Key_Index (Atree : A_Tree;
                                Index : Valid_Key_Index)
                          return Value_Type with
     Pre => Populated (Atree) and then Index <= Count (Atree),
     Ghost;

   --                    A_Tree Subprograms                            --
   -----------------------------------------------------------------------

   function Tree_Depth (Atree : A_Tree) return Natural;

   function Is_Present (Atree : A_Tree; Key : Key_Type)
                        return Boolean with
     Pre  => Populated (Atree) and Ordered (Atree),
     Post => (if Is_Present'Result then
                (for some I in Valid_Key_Index range
                     First_Index .. Count (Atree) =>
                     (Indexed_Key (Atree, I) = Key)));

   function Value (Atree : A_Tree; Key : Key_Type)
                   return Value_Type with
     Pre  => Populated (Atree) and Ordered (Atree),
     Post => (if Value'Result /= Null_Value then
                (for some I in Valid_Key_Index range
                     First_Index .. Count(Atree) =>
                     Indexed_Key (Atree, I) = Key and then
                   Value_At_Key_Index (Atree, I) = Value'Result));

   function Equal_Keys (Atree_1, Atree_2 : A_Tree) return Boolean with
   Pre  => Populated (Atree_1) and Populated (Atree_2) and
            Ordered (Atree_1) and Ordered (Atree_2),
     Post =>  (if Equal_Keys'Result then
          (Count (Atree_1) = Count (Atree_2)) and then
          (for all I in Valid_Key_Index range First_Index .. Count (Atree_1) =>
                  (Indexed_Key (Atree_1, I) =
                     Indexed_Key (Atree_2, I))));

   function Equal_Keys_And_Values (Atree_1, Atree_2 : A_Tree;
                                   Host_1, Host_2 : Host_Tree)
              return Boolean with
     Pre  => Populated (Atree_1) and Populated (Atree_2) and
              Ordered (Atree_1) and Ordered (Atree_2),
     Post => Equal_Keys_And_Values'Result =
              (Count (Atree_1) = Count (Atree_2)) and then
                 (for all I in Valid_Key_Index range
                     First_Index .. Count (Atree_1) =>
                       (Indexed_Key (Atree_1, I) =
                        Indexed_Key (Atree_2, I) and
                        Value_At_Key_Index (Atree_1, I) =
                        Value_At_Key_Index (Atree_2, I)));

   procedure New_A_Tree (Atree : out A_Tree; The_Host : in out Host_Tree) with
     Post => Host (Atree) = The_Host and then
             Count (Atree) = 0 and then Ordered (Atree);

   procedure Insert (Atree     : in out A_Tree;
                     Key       : Key_Type;
                     Inserted  : out Boolean) with
     Pre  => Count (Atree) < Natural'Last and then Ordered (Atree),
     Post => Host (Atree) = Host (Atree'Old) and then Populated (Atree) and then
             Ordered (Atree) and then
             (for some I in Valid_Key_Index range
                 First_Index .. Count (Atree) =>
                     Indexed_Key (Atree, I) = Key) and then
             (if Inserted then
                 Count (Atree) = Count (Atree'Old) + 1
               else
                 Count (Atree) = Count (Atree'Old));

   procedure Insert_With_Value (Atree         : in out A_Tree;
                                Key           : Key_Type;
                                Insert_Value  : Value_Type;
                                Inserted      : out Boolean;
                                Value_At_Node : out Value_Type) with
     Pre  => Count (Atree) < Natural'Last and then Ordered (Atree),
     Post => Host (Atree) = Host (Atree'Old) and then Populated (Atree) and then
             Ordered (Atree) and then Value_At_Node = Value (Atree, Key) and then
             (for some I in Valid_Key_Index range
                 First_Index .. Count (Atree) =>
                     Indexed_Key (Atree, I) = Key) and then
            (if Inserted then
                Count (Atree) = Count (Atree'Old) + 1 and
                Value_At_Node = Insert_Value
              else
                Count (Atree) = Count (Atree'Old));

   ------------ Enumerators for Atree depth first traversal ---------------
   type Enumerator is private;

   function Enumerated_Tree (E : Enumerator) return A_Tree with Ghost;

   function Current_Key_Index (E: Enumerator) return Key_Index with
     Post => Current_Key_index'Result <= Count (Enumerated_Tree (E)),
     Ghost;

   function New_Enumerator (Atree : A_Tree) return Enumerator with
     Pre  => Populated (Atree),
     Post => Enumerated_Tree (New_Enumerator'Result) = Atree and then
             Current_Key_Index (New_Enumerator'Result) = 1;

   procedure Next_Key (E : in out Enumerator;
                       Key : out Key_Type) with
     Pre  => Populated (Enumerated_Tree (E)) and then
             Ordered (Enumerated_Tree (E)),
     Post => Enumerated_Tree (E) = Enumerated_Tree (E'Old) and then
             (if Current_Key_Index (E'Old) in
                First_Index .. Count (Enumerated_Tree (E'Old)) - 1
              then
                Key = Indexed_Key (Enumerated_Tree (E'Old),
                                   Current_Key_Index (E'Old)) and
                Current_Key_Index (E) = Current_Key_Index (E'Old) + 1
                  else
                    Current_Key_Index (E) = Null_Key_Index and
                    Key = Null_Key);


   procedure Next_Key_And_Value (E         : in out Enumerator;
                                 Key       : out Key_Type;
                                 Its_Value : out Value_Type) with
     Pre  => Populated (Enumerated_Tree (E)) and then
             Ordered (Enumerated_Tree (E)),
     Post => Enumerated_Tree (E) = Enumerated_Tree (E'Old) and then
             (if Current_Key_Index (E) in
                First_Index .. Count (Enumerated_Tree (E'Old)) - 1
              then
                Key = Indexed_Key (Enumerated_Tree (E'Old),
                                   Current_Key_Index (E'Old)) and
                Its_Value = Value (Enumerated_Tree (E'Old), Key) and
                Current_Key_Index (E) = Current_Key_Index (E'Old) + 1
                  else
                    Current_Key_Index (E) = Null_Key_Index and
                    Key = Null_Key and
                    Its_Value = Null_Value);

private
   subtype Node_Index is Natural range 0 .. Max_Nodes_In_Tree;
   subtype Valid_Node_Index is Node_Index range  1 .. Node_Index'Last;
   Null_Index : constant Node_Index := 0;


      --  Direction - Right = True, Left = False;
   Right : constant Boolean := True;
   Left  : constant Boolean := False;



   package Tree is new Basic_Tree
     (Node_Index => Node_Index,
      Level_Type => Natural,
      Key_Type   => Key_Type,
      Value_Type => Value_Type,
      Null_Key   => Null_Key,
      Null_Value => Null_Value);

   type Host_Tree is new Tree.Tree;

   type A_Tree is
      record
         Host  : Tree.Tree;
         Count : Key_Count;
         Base  : Valid_Node_Index;
         Root  : Node_Index;
      end record;

   --  A logical function to state that a Node_Index references a node
   --  within the given A_Tree.
   function In_Atree (Atree : A_Tree; Node : Node_Index) return Boolean with
     Ghost;

   --  The Stack_Size must be large enough to traverse the tree without
   --  overflow.
   --  The minimum Stack_Size can be calculated from the maximum nodes, N,
   --  in the Atree.
   --  As the Atree will be balanced, the minimum Stack_Size must be greater
   --  than the maximum height of the tree, K.
   --  K = Log2 (N + 1) - 1.

   package Stack is new Bounded_Stacks
     (Element_Type => Valid_Node_Index,
      Stack_Size   => Stack_Size);

   type Enumerator is
      record
         Atree      : A_Tree;
         Key_Issue  : Key_Count;
         --  A stack to record visited nodes when enumerating.
         Visited : Stack.Stack;
      end record with
     Type_Invariant => Key_Equivalence (Enumerator);

   function Key_Equivalence (E : Enumerator) return Boolean with Ghost;

   function Current_Node_Index (E    : Enumerator) return Node_Index with
     Pre => not Stack.Is_Empty (E.Visited);

end Atrees;
