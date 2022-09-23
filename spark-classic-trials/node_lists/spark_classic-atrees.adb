package body SPARK_Classic.Atrees is

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree (Tree : A_Tree) return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Empty_Tree unimplemented");
      raise Program_Error with "Unimplemented function Empty_Tree";
      return Empty_Tree (Tree => Tree);
   end Empty_Tree;

   --------------
   -- New_Tree --
   --------------

   procedure New_Tree
     (Tree : out A_Tree)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Tree unimplemented");
      raise Program_Error with "Unimplemented procedure New_Tree";
   end New_Tree;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Tree       : in out A_Tree;
      Key        : Key_Type;
      Tree_Store : in out Trees.Tree_Type;
      Inserted   : out Boolean)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present
     (Tree       : A_Tree;
      Key        : Key_Type;
      Tree_Store : Trees.Tree_Type)
      return Boolean
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Is_Present unimplemented");
      raise Program_Error with "Unimplemented function Is_Present";
      return Is_Present (Tree => Tree, Key => Key, Tree_Store => Tree_Store);
   end Is_Present;

   ----------------
   -- Tree_Depth --
   ----------------

   function Tree_Depth
     (Tree       : A_Tree;
      Tree_Store : Trees.Tree_Type)
      return Natural
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Tree_Depth unimplemented");
      raise Program_Error with "Unimplemented function Tree_Depth";
      return Tree_Depth (Tree => Tree, Tree_Store => Tree_Store);
   end Tree_Depth;

   --------------------
   -- New_Enumerator --
   --------------------

   function New_Enumerator
     (Tree       : A_Tree;
      Tree_Store : Trees.Tree_Type)
      return Enumerator
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Enumerator unimplemented");
      raise Program_Error with "Unimplemented function New_Enumerator";
      return New_Enumerator (Tree => Tree, Tree_Store => Tree_Store);
   end New_Enumerator;

   --------------
   -- Next_Key --
   --------------

   procedure Next_Key (E : in out Enumerator; Key : out Key_Type) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Next_Key unimplemented");
      raise Program_Error with "Unimplemented procedure Next_Key";
   end Next_Key;

   ----------------
   -- Next_Value --
   ----------------

   procedure Next_Value (E : in out Enumerator; Value: out Value_Type) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Next_Value unimplemented");
      raise Program_Error with "Unimplemented procedure Next_Value";
   end Next_Value;

end SPARK_Classic.Atrees;
