package List_Type_Own_Pragma is
   pragma Classic_Own (((Plain => V, Plain => W, Plain => X) with Own_Type => Integer));
   V : Integer;
end List_Type_Own_Pragma;
