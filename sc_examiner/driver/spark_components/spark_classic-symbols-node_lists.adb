package body SPARK_Classic.Symbols.Node_Lists
is

   --------------
   -- New_List --
   --------------

   procedure New_List
     (N_List : out Table_Ref)
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
      N_List : Table_Ref)
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
     (N_List : in out Table_Ref)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Save_List unimplemented");
      raise Program_Error with "Unimplemented procedure Save_List";
   end Save_List;

   ----------
   -- Find --
   ----------

   function Find
     (N : Types.Node_Id;
      N_List : Table_Ref)
      return Table_Ref
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Find unimplemented");
      raise Program_Error with "Unimplemented function Find";
      return Find (N => N, N_List => N_List);
   end Find;

end SPARK_Classic.Symbols.Node_Lists;
