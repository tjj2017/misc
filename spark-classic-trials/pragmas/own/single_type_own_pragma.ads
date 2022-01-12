package Single_Type_Own_Pragma is
   pragma Classic_Own (((Plain => V) with Own_Type => Integer));
   V : Integer;
end Single_Type_Own_Pragma;
