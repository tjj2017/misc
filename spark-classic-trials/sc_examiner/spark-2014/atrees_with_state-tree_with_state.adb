with Basic_Tree;
package body Atrees_With_State.Tree_With_State with
SPARK_Mode => Off
is
   package Tree is new Basic_Tree
     (Tree_Node  => Tree_Node,
      Level_Type => Level_Type,
      Key_Type   => Key_Type,
      Value_Type => Value_Type,
      Null_Key   => Null_Key,
      Null_Value => Null_Value);

   function Is_Empty_Tree return Boolean renames Tree.Is_Empty_Tree;

   function Is_A_Valid_Tree_Node (N : Tree_Node) return Boolean renames
     Tree.Is_A_Valid_Tree_Node;

   function First_Node_In_Tree return Tree_Node renames
     Tree.First_Node_In_Tree;

   function Last_Node_In_Tree return Tree_Node renames
     Tree.Last_Node_In_Tree;

   function In_Tree  (N : Tree_Node) return Boolean renames
     Tree.In_Tree;

   function Level (N : Valid_Tree_Node) return Level_Type renames
     Tree.Level;

   function Left  (N : Valid_Tree_Node) return Tree_Node renames
     Tree.Left;

   function Right (N : Valid_Tree_Node) return Tree_Node renames
     Tree.Right;

   function Key (N : Valid_Tree_Node) return Key_Type renames
     Tree.Key;

   function Value (N : Valid_Tree_Node) return Value_Type renames
     Tree.Value;

   procedure Init renames
     Tree.Init;

   procedure Set_Level (N : Valid_Tree_Node; Node_Level : Level_Type) renames
     Tree.Set_Level;

   procedure Set_Left  (N : Valid_Tree_Node; Branch : Tree_Node) renames
     Tree.Set_Left;

   procedure Set_Right (N : Valid_Tree_Node; Branch : Tree_Node) renames
     Tree.Set_Right;

   procedure Set_Key (N : Valid_Tree_Node; The_Key : Key_Type) renames
     Tree.Set_Key;

   procedure Set_Value (N : Valid_Tree_Node; Node_Value : Value_Type) renames
     Tree.Set_Value;

   procedure Add_Node  (N : out Valid_Tree_Node; The_Key : Key_Type) renames
     Tree.Add_Node;

   procedure Clear_Tree_Below_Node (N : Valid_Tree_Node) renames
     Tree.Clear_Tree_Below_Node;

   function Node_Contents (N : Tree_Node) return Node_Abstraction is
     (if Is_A_Valid_Tree_Node (N) then
           Node_Abstraction'
        (Key   => Key (N),
         Value => Value (N),
         Level => Level (N),
         Left  => Left (N),
         Right => Right (N))
      else
         Null_Node_Abstraction);

   function Tree_Contents_To (N : Tree_Node) return Tree_Abstraction is
      Result : Tree_Abstraction (First_Node_In_Tree .. N);
   begin
      for Node in Result'Range loop
         Result (Node) := Node_Contents (Node);
      end loop;
      return Result;
   end Tree_Contents_To;

end Atrees_With_State.Tree_With_State;
