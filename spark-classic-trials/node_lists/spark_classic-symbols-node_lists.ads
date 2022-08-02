with SPARK_Classic.Symbols.List_Store;
private package SPARK_Classic.Symbols.Node_Lists
--  --# own In_Progress;
--  --# initializes In_Progress;
with Abstract_State => In_Progress,
     Initializes => In_Progress,
     Initial_Condition => not Building_List
is
   type Node_List is limited private;

   function Building_List return Boolean
   --  --# global In_Progress;
   with Global => In_Progress;

   procedure New_List (Root : out Node_List)
     --  --# global in out SPARK_Classic.Symbols.List_Store.Store;
     --  --#           out In_Progress;
   --  --# pre not Building_List;
   --  --# post Building_List;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store,
                   Output => In_Progress),
        Pre => not Building_List,
        Post => Building_List;

   procedure Insert (N : Types.Node_Id; Root : Node_List)
   --  --# global in out Store;
   --  --# pre Building_List;
   --  --# post Building_List;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store),
        Pre => Building_List,
        Post => Building_List;

   procedure Save_List (Root : in out Node_List)
   --  --# global in out Store;
   --  --#           out In_Progress;
   --  --# pre Building_List;
   --  --# post not Building_List;
   with Global => (In_Out => SPARK_Classic.Symbols.List_Store.Store,
                   Output => In_Progress),
        Pre => Building_List,
        Post => not Building_List;

   function Is_Present (N : Types.Node_Id; Root : Node_List) return Boolean
   --  --# global in SPARK_Classic.Symbols.List_Store.Store;
   with Global => (Input => SPARK_Classic.Symbols.List_Store.Store);

private
   type Node_List is range Table_Index'First .. Table_Index'Last;

end SPARK_Classic.Symbols.Node_Lists;
