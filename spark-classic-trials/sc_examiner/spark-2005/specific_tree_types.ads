package Specific_Tree_Types is
   subtype Tree_Node is Natural;          -- type range <>;
   subtype Valid_Tree_Node is Tree_Node range
     Tree_Node'Succ (Tree_Node'First) .. Tree_Node'Last;
   subtype Level_Type is Natural;         -- type range <>;
   subtype Key_Type is Tree_Node;         -- type (<>);
   subtype Value_Type is Integer;         -- type private;
   Null_Key : constant Key_Type := 0;
   Null_Value : constant Value_Type := 0;
   Stack_Size : constant Positive := 32;
end Specific_Tree_Types;
