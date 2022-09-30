pragma Ada_2012;
package body SPARK_Classic.Symbols.Node_Trees
with Refined_State => (Store => (Tree_Store, In_Progress))
--  --# own Store is Tree_Store, In_Progress;
is
   In_Progress : Boolean;
   Tree_Store : Atrees.Trees.Tree_Type;

   ----------------------
   -- Initialize_Store --
   ----------------------

   procedure Initialize_Store
   is
   begin
      Tree_Store.New_Tree;
      In_Progress := False;
   end Initialize_Store;

   -------------------
   -- Building_List --
   -------------------

   function Building_List
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Building_List unimplemented");
      return raise Program_Error with "Unimplemented function Building_List";
   end Building_List;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List (List : Node_List) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Empty_List unimplemented");
      return raise Program_Error with "Unimplemented function Empty_List";
   end Empty_List;

   --------------
   -- New_List --
   --------------

   procedure New_List
     (List : out Node_List)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_List unimplemented");
      raise Program_Error with "Unimplemented procedure New_List";
   end New_List;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (N : Types.Node_Id;
      List : in out Node_List;
      Inserted : out Boolean)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   ---------------
   -- Save_List --
   ---------------

   procedure Save_List
     (List : in out Node_List)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Save_List unimplemented");
      raise Program_Error with "Unimplemented procedure Save_List";
   end Save_List;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present
     (N : Types.Node_Id;
      List : Node_List)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Is_Present unimplemented");
      return raise Program_Error with "Unimplemented function Is_Present";
   end Is_Present;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth
     (N_List : Node_List)
      return Natural
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Tree_Depth unimplemented");
      return raise Program_Error with "Unimplemented function Tree_Depth";
   end Tree_Depth;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator
     (N_List : Node_List)
      return Enumerator
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Enumerator unimplemented");
      return raise Program_Error with "Unimplemented function New_Enumerator";
   end New_Enumerator;

   ----------
   -- Next --
   ----------

   procedure Next
     (E : in out Enumerator;
      Next_Value: out Types.Node_Id)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Next unimplemented");
      raise Program_Error with "Unimplemented procedure Next";
   end Next;

end SPARK_Classic.Symbols.Node_Trees;
