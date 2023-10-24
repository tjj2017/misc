with SPARK_2014.Dynamic_Tables;
generic
   type Key_Type is (<>);
   type Value_Type is private;
   Null_Value : Value_Type;
package SPARK_2014.Trees is
   type Tree_Node is private;
   Empty_Node : constant Tree_Node;
   function Is_A_Node (N : Tree_Node) return Boolean
     with Post => Is_A_Node'Result = ( N /= Empty_Node),
          Ghost;

   type Tree_Type is private;
   type Persist_Node is private;
   function Is_Empty_Tree (T : Tree_Type) return Boolean;
   function Persist_Contents (T : Tree_Type; N : Tree_Node) return Persist_Node
     with Pre => In_Tree (T, N),
     Ghost;

   subtype Model_Index is Natural range 1 .. Natural'Last - 1;
   type Model_Node is private with Ghost;
   type Tree_Model is array (Model_Index range  <>) of Model_Node with Ghost;
   function Is_Empty_Model (M :Tree_Model) return Boolean with Ghost;
   function To_Tree_Node (N : Model_Index) return Tree_Node with Ghost;
   function To_Persist_Node (M_Node : Model_Node) return Persist_Node with Ghost;
   function In_Model (M : Tree_Model; N : Tree_Node) return Boolean with Ghost;

   function In_Tree  (T : Tree_Type; N : Tree_Node) return Boolean with
     Post => (In_Tree'Result = (not Is_Empty_Tree (T) and
                  N /= Empty_Node));

   function Model_Equivalence (T : Tree_Type; M : Tree_Model) return Boolean is
     (for all I in M'Range =>
         To_Tree_Node (I) /= Empty_Node and
         In_Tree (T, To_Tree_Node (I)) and
        In_Model (M, To_Tree_Node (I)) and
          In_Tree (T, To_Tree_Node (I)) and
        To_Persist_Node (M (I)) =
          Persist_Contents (T, To_Tree_Node (I)))
     with Pre => (for all I in M'Range =>
                    In_Tree (T, To_Tree_Node (I))),
          Ghost;

   function To_Model (T : Tree_Type) return Tree_Model with
     Post => Model_Equivalence (T, To_Model'Result),
     Ghost;

   function Persists (T_Pre, T_Post : Tree_Model) return Boolean
     with Pre => T_Pre'First = T_Post'First and T_Pre'Last = T_Post'Last,
          Ghost;

   function New_Persists (T_Pre : Tree_Model; T_Post : Tree_Type) return Boolean
     with
       Pre => not Is_Empty_Model (T_Pre) and not Is_Empty_Tree (T_Post) and
              (for all I in T_Pre'Range => In_Tree (T_Post, To_Tree_Node (I))),
      Post => New_Persists'Result =
              (for all I in T_Pre'Range =>
                 In_Model (T_Pre, To_Tree_Node (I)) =
                   In_Tree (T_Post, To_Tree_Node (I))
               and
                 In_Tree (T_Post, To_Tree_Node (I)) and
                 To_Persist_Node (T_Pre (I)) =
                   Persist_Contents (T_Post, To_Tree_Node (I))),
       Ghost;

   function Node_Is_Present  (T : Tree_Type; N : Tree_Node) return Boolean;
   function Key_Is_Present (T : Tree_Type; K : Key_Type) return Boolean
     with Pre => not Is_Empty_Tree (T),
     Ghost;

   procedure New_Tree (T : out Tree_Type);

   function Level (T : Tree_Type; N : Tree_Node) return Natural
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N);
   function Left  (T : Tree_Type; N : Tree_Node) return Tree_Node
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N),
          Post => (if Is_A_Node (Left'Result) then
                     In_Tree (T, Left'Result));
   function Right (T : Tree_Type; N : Tree_Node) return Tree_Node
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N),
          Post => (if Is_A_Node (Right'Result) then
                     In_Tree (T, Right'Result));
   function Key (T : Tree_Type; N : Tree_Node) return Key_Type
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N);
   function Value (T : Tree_Type; N : Tree_Node) return Value_Type
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N);

   procedure Set_Level (T : in out Tree_Type; N : Tree_Node;
                        Node_Level : Natural)
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N)
                  and not Is_Empty_Model (To_Model (T)),
     Post => In_Tree (T, N) and
             not Is_Empty_Tree (T) and
                  New_Persists (To_Model (T)'Old, T) and
             In_Tree (T, N) and Level (T, N) = Node_Level;

   procedure Set_Left  (T : in out Tree_Type; N : Tree_Node;
                        Branch : Tree_Node)
     with  Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N)
                  and not Is_Empty_Model (To_Model (T)),
           Post => In_Tree (T, N) and
                     not Is_Empty_Tree (T) and
                     New_Persists (To_Model (T)'Old, T) and
                     In_Model (To_Model (T)'Old, Branch) = In_Tree (T, Branch)
                     and Left (T, N) = Branch and
                     Model_Equivalence (T, To_Model (T));

   procedure Set_Right (T : in out Tree_Type; N : Tree_Node;
                        Branch : Tree_Node)
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N)
                  and not Is_Empty_Model (To_Model (T)),
          Post => In_Tree (T, N) and
                  not Is_Empty_Tree (T) and
                  New_Persists (To_Model (T)'Old, T) and
                  In_Model (To_Model (T)'Old, Branch) = In_Tree (T, Branch)
                  and Right (T, N) = Branch and
                  Model_Equivalence (T, To_Model (T));

   procedure Set_Key (T : in out Tree_Type; N : Tree_Node;
                      The_Key : Key_Type)
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N)
                 and not Is_Empty_Model (To_Model (T)),
      Post => In_Tree (T, N) and
             not Is_Empty_Tree (T) and
                  New_Persists (To_Model (T)'Old, T) and
                    Key_Is_Present (T, The_Key) and
                      Key (T, N) = The_Key;

   procedure Set_Value (T : in out Tree_Type; N : Tree_Node;
                        Node_Value : Value_Type)
     with Pre => N /= Empty_Node and not Is_Empty_Tree (T) and In_Tree (T, N)
                 and not Is_Empty_Model (To_Model (T)),
          Post => not Is_Empty_Tree (T) and
                  New_Persists (To_Model (T)'Old, T) and
                  Value (T, N) = Node_Value;

   procedure Add_Node  (T : in out Tree_Type; N : out Tree_Node;
                        The_Key : Key_Type)
     with Post => not Is_Empty_Tree (T) and
                  In_Tree (T, N) and
                  In_Model (To_Model (T), N) and
                  not Is_Empty_Model (To_Model (T)) and
                   New_Persists (To_Model (T)'Old,T) and
                  Key_Is_Present (T, The_Key) and
                  Key (T, N) = The_Key and
                  Left (T, N) = Empty_Node and
                  Right (T, N) = Empty_Node and
                  Value (T, N) = Null_Value;

   procedure Clear (T : in out Tree_Type; N : Tree_Node)
     with Post => not In_Tree (T, N);

private
   type Tree_Node is range 0 .. Natural'Last - 1;
   subtype Valid_Tree_Node is Tree_Node range 1 .. Tree_Node'Last;
   Empty_Node : constant Tree_Node := 0;

   type Persist_Node is
      record
         Key   : Key_Type;
         Value : Value_Type;
      end record;

   type Actual_Node is
      record
         Conts : Persist_Node;
         Level : Natural;
         Left  : Tree_Node;
         Right : Tree_Node;
      end record;

   Null_Actual_Node : constant Actual_Node := Actual_Node'
     (Conts => Persist_Node'(Key => Key_Type'First, Value => Null_Value),
      Level => 0,
      Left  => Empty_Node,
      Right => Empty_Node);

   package Dynamic_Tables is new SPARK_2014.Dynamic_Tables
     (Table_Component_Type => Actual_Node,
      Table_Index_Type     => Valid_Tree_Node,
      Table_Low_Bound      => 1,
      Table_Initial        => 32,
      Table_Increment      => 100);

   type Tree_Type is
      record
         The_Tree : Dynamic_Tables.Table_Type;
      end record;

   type Model_Node is new Persist_Node;
end SPARK_2014.Trees;
