with Types;
package Specific_Tree_Types is
   subtype Tree_Node is Types.Node_Id;   -- type range <>;
   subtype Level_Type is Natural;         -- type range <>;
   subtype Key_Type is Types.Node_Id;     -- type (<>);
   subtype Value_Type is Integer;         -- type private;
   Null_Key : constant Key_Type := 0;
   Null_Value : constant Value_Type := 0;
   Stack_Size : constant Positive := 32;
end Specific_Tree_Types;
