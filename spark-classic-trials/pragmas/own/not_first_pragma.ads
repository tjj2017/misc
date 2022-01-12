package Not_First_Pragma is
   pragma Classic_Initializes ((V));
   pragma Classic_Own (((Plain => V) with Own_Type => Integer));
end Not_First_Pragma;
