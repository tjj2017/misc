with Types,
     SPARK_Classic.Symbols.Nodes,
     Ada.Text_IO;
use Ada.Text_IO;
use type Types.Node_Id;
procedure Numbers_Reversed is
   subtype Index is Positive range 1 .. 7;
   type Node_Array is array (Index) of Types.Node_Id;
   N_1 : constant Node_Array := Node_Array'
     (1, 2, 3, 4, 5, 6, 7);
   N_2 : constant Node_Array := Node_Array'
     (99, 6, 5, 4, 3, 2, 1);
   N_List_1 : SPARK_Classic.Symbols.Nodes.Node_List;
   N_List_2 : SPARK_Classic.Symbols.Nodes.Node_List;
   Enum_1   : SPARK_Classic.Symbols.Nodes.Enumerator;
   Enum_2   : SPARK_Classic.Symbols.Nodes.Enumerator;
   Next     : Types.Node_Id;
begin
   SPARK_Classic.Symbols.Nodes.Initialize_Store;
   SPARK_Classic.Symbols.Nodes.New_List (N_List_1);
   for I in Index loop
      SPARK_Classic.Symbols.Nodes.Insert (N_1 (I), N_List_1);
   end loop;
   Put_Line ("Depth of tree N_1: " & Integer'Image
        (SPARK_Classic.Symbols.Nodes.Tree_Depth (N_List_1)));
   Enum_1 := SPARK_Classic.Symbols.Nodes.New_Enumerator (N_List_1);
   SPARK_Classic.Symbols.Nodes.Next (Enum_1, Next);

   Put_Line ("Enumeration of N_List_1");
   while Next /= Types.Empty loop
      Put (Types.Node_Id'Image (Next) & " ");
      SPARK_Classic.Symbols.Nodes.Next (Enum_1, Next);
   end loop;
   New_Line;

   SPARK_Classic.Symbols.Nodes.New_List (N_List_2);
   for I in Index loop
       SPARK_Classic.Symbols.Nodes.Insert (N_2 (I), N_List_2);
   end loop;
   Put_Line ("Depth of tree N_2: " & Integer'Image
        (SPARK_Classic.Symbols.Nodes.Tree_Depth (N_List_2)));
   Enum_2 := SPARK_Classic.Symbols.Nodes.New_Enumerator (N_List_2);
   SPARK_Classic.Symbols.Nodes.Next (Enum_2, Next);

   Put_Line ("Enumeration of N_List_2");
   while Next /= Types.Empty loop
      Put (Types.Node_Id'Image (Next) & " ");
      SPARK_Classic.Symbols.Nodes.Next (Enum_2, Next);
   end loop;
   New_Line;

   Enum_1 := SPARK_Classic.Symbols.Nodes.New_Enumerator (N_List_1);
   SPARK_Classic.Symbols.Nodes.Next (Enum_1, Next);
   Put_Line ("Enumeration of N_List_1");
   while Next /= Types.Empty loop
      Put (Types.Node_Id'Image (Next) & " ");
      SPARK_Classic.Symbols.Nodes.Next (Enum_1, Next);
   end loop;
   New_Line;

end Numbers_Reversed;
