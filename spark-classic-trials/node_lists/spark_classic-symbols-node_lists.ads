with SPARK_Classic.Symbols.List_Store;
private package SPARK_Classic.Symbols.Node_Lists
--  --# own In_Progress;
--  --# initializes In_Progress;
with Abstract_State => In_Progress,
     Initializes => In_Progress,
     Initial_Condition => not Building_List
is
   type Node_List is private;

   Null_Node_List : constant Node_list;

   function Building_List return Boolean
   --  --# global In_Progress;
   with Global => In_Progress;

   procedure New_List (List : out Node_List)
     --  --# global in out List_Store.Store;
     --  --#           out In_Progress;
   --  --# pre not Building_List;
   --  --# post Building_List;
   with Global => (In_Out => List_Store.Store,
                   Output => In_Progress),
        Pre => not Building_List,
        Post => Building_List;

   procedure Insert (N : Types.Node_Id; List : in out Node_List)
   --  --# global in out List_Sore.Store;
   --  --# pre Building_List;
   --  --# post Building_List;
   with Global => (In_Out => List_Store.Store),
        Pre => Building_List,
        Post => Building_List;

   procedure Save_List (List : in out Node_List)
   --  --# global in out Store;
   --  --#           out In_Progress;
   --  --# pre Building_List;
   --  --# post not Building_List;
   with Global => (In_Out => List_Store.Store,
                   Output => In_Progress),
        Pre => Building_List,
        Post => not Building_List;

   function Is_Present (N : Types.Node_Id; List : Node_List) return Boolean
   --  --# global in List_Store.Store;
   with Global => (Input => List_Store.Store);

private
   type Node_List is
      record
         Root : Table_Index;
      end record;

   Null_Node_List : constant Node_List := Node_List'(Root => No_Index);

end SPARK_Classic.Symbols.Node_Lists;
